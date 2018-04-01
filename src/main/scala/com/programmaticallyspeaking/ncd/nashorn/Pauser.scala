package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.StackFrameImpl
import com.sun.jdi.event._
import org.slf4s.Logging

class Pauser(breakpoints: ActiveBreakpoints, scripts: Scripts, emitter: ScriptEventEmitter) extends Logging {

  private var currentExceptionPauseType: ExceptionPauseType = ExceptionPauseType.None

  /**
    * By default, we don't pause when a breakpoint is hit. This is important since we add a fixed breakpoint for
    * JS 'debugger' statements, and we don't want that to pause the VM when a debugger hasn't attached yet.
    */
  private var willPauseOnBreakpoints = false

  /**
    * If true, we won't stop on breakpoints or exceptions.
    */
  private var disablePausingAltogether = false

  def pauseOnBreakpoints(pause: Boolean): Unit = {
    willPauseOnBreakpoints = pause
  }

  /**
    * Changes the exception pause type.
    *
    * @param pauseType the new exception pause type
    * @return `true` if the type was changed, otherwise `false`
    */
  def pauseOnExceptions(pauseType: ExceptionPauseType): Boolean = {
    val oldValue = currentExceptionPauseType
    currentExceptionPauseType = pauseType
    pauseType != oldValue
  }

  def disablePausing(disable: Boolean): Unit = {
    disablePausingAltogether = disable
  }

  private def describeBreakpointId(ev: LocatableEvent): String = ev match {
    case be: BreakpointEvent => ActiveBreakpoint.getBreakpointId(be).map(s => s" $s").getOrElse("")
    case _ => ""
  }

  /**
    * RETURN TRUE TO RESUME
    * RETURN FALSE TO PAUSE
    */
  def handleBreakpoint(ev: LocatableEvent, pausedData: PausedData): Boolean = shouldPause(pausedData, ev) match {
    case Left(reason) =>
      log.debug(s"Ignoring breakpoint${describeBreakpointId(ev)} at ${ev.location()} because $reason.")
      true
    case Right(_) =>
      val reason: BreakpointReason = pausedData.exceptionEventInfo match {
        case Some(info) =>
          info.marshalledException match {
            case Right(e) => BreakpointReason.Exception(Some(e))
            case Left(str) =>
              log.debug("No exception data: " + str)
              BreakpointReason.Exception(None)
          }
        case None =>
          ev match {
            case _ if pausedData.isAtDebuggerStatement =>
              BreakpointReason.Debugger
            case _: BreakpointEvent =>
              BreakpointReason.Breakpoint
            case _ =>
              BreakpointReason.Step
          }
      }

      // Log at debug level because we get noise due to exception requests.
      val details = ev match {
        case ex: ExceptionEvent =>
          s" (due to exception ${ex.exception().referenceType().name()})"
        case _ if pausedData.isAtDebuggerStatement =>
          s" (at a JavaScript 'debugger' statement)"
        case _ if reason == BreakpointReason.Step => " (due to stepping)"
        case _ => ""
      }

      log.debug(s"Pausing at breakpoint${describeBreakpointId(ev)} at location ${ev.location()} in thread ${ev.thread().name()}$details")

      // Resume will be controlled externally
      implicit val marshaller = pausedData.marshaller
      !doPause(pausedData.stackFrames, reason)
  }

  /**
    * RETURN TRUE TO PAUSE
    * RETURN FALSE TO RESUME
    */
  private def doPause(stackFrames: Seq[StackFrame], reason: BreakpointReason)(implicit marshaller: Marshaller): Boolean = {
    stackFrames.headOption.collect { case sf: StackFrameImpl => sf } match {
      case Some(topStackFrame) =>
        val scriptId = topStackFrame.scriptId
        breakpoints.activeFor(topStackFrame.breakableLocation) match {
          case Some(activeBreakpoint) if reason == BreakpointReason.Breakpoint =>
            val breakpointId = activeBreakpoint.id

            // Check condition if we have one. We cannot do this until now (which means that a conditional breakpoint
            // will be slow) because we need stack frames and locals to be setup for code evaluation.
            val conditionIsTrue = activeBreakpoint.condition match {
              case Some(c) =>
                topStackFrame.eval(c, None) match {
                  case SimpleValue(true) => true
                  case SimpleValue(false) =>
                    log.trace(s"Not pausing on breakpoint $breakpointId in script $scriptId since the condition ($c) evaluated to false.")
                    false
                  case other =>
                    log.warn(s"Condition $c resulted in unexpected value $other, will pause.")
                    // It's best to pause since we don't know what happened, I think.
                    true
                }
              case None => true // no condition, always stop
            }

            if (conditionIsTrue) {
              scripts.byId(ScriptIdentity.fromId(scriptId)).foreach { s =>
                val location = topStackFrame.location
                val line = s.sourceLine(location.lineNumber1Based).getOrElse("<unknown line>")
                log.info(s"Pausing at breakpoint $breakpointId at ${s.url}:${location.lineNumber1Based}: $line")
              }

              val hitBreakpoint = HitBreakpoint(stackFrames, Some(breakpointId), BreakpointReason.Breakpoint)
              emitter.emit(hitBreakpoint)
            }
            conditionIsTrue
          case _ =>
            // debugger statement, or exception
            emitter.emit(HitBreakpoint(stackFrames, None, reason))
            true
        }
      case None =>
        log.debug("Not pausing because there are no stack frames")
        false
    }
  }

  private def shouldPauseOnException(exceptionEventInfo: ExceptionEventInfo): Either[String, Unit] = {
    val pausingAtAll = currentExceptionPauseType != ExceptionPauseType.None
    val pausingOnAllExceptions = currentExceptionPauseType == ExceptionPauseType.All
    def exceptionType = exceptionEventInfo.exceptionType match {
      case ExceptionType.Unknown if !pausingOnAllExceptions =>
        // We have to determine whether to pause or not; the exception may be caught or uncaught but
        // we don't know which.
        // The safest approach seems to be to pause. Chrome/DevTools wants uncaught or all exceptions,
        // never only caught ones (as of this writing), so the worst thing that can happen is that we
        // pause on a caught exception even though we shouldn't.
        log.warn(s"Cannot determine caught-ness of ${exceptionEventInfo.exceptionTypeName} thrown at ${exceptionEventInfo.throwLocation}, will assume uncaught.")
        ExceptionType.UncaughtByScript
      case other => other
    }

    val shouldPause = pausingAtAll && (pausingOnAllExceptions || (exceptionType match {
      case ExceptionType.CaughtByScript => currentExceptionPauseType == ExceptionPauseType.Caught
      case ExceptionType.UncaughtByScript => currentExceptionPauseType == ExceptionPauseType.Uncaught
      case _ => false
    }))

    if (shouldPause) Right(())
    else Left(s"exception is $exceptionType, which is ignored")
  }

  private def shouldPause(pausedData: PausedData, ev: Event): Either[String, Unit] = ev match {
    case _ if disablePausingAltogether =>
      // disablePausingAltogether disabled all sort of pausing - exceptions, stepping, breakpoints...
      Left("pausing is entirely disabled")
    case _ if pausedData.exceptionEventInfo.isDefined =>
      shouldPauseOnException(pausedData.exceptionEventInfo.get)
    case _:StepEvent|_:MethodEntryEvent =>
      // Stepping should work even if breakpoints are disabled, and method entry is when the user wants to pause,
      // which also should work when breakpoints are disabled.
      Right(())
    case _ =>
      // Resume right away if we're not pausing on breakpoints
      if (!willPauseOnBreakpoints) return Left("breakpoints are disabled")
      if (pausedData.stackFrameHolders.isEmpty) return Left("no stack frames were found at all")
      if (!pausedData.pausedInAScript) return Left("location doesn't belong to a script")

      Right(())
  }

}
