package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ExceptionPauseType
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{StepRequestClassFilter, isInfrastructureThread, isRunningThread}
import com.sun.jdi.StackFrame
import com.sun.jdi.event.Event
import com.sun.jdi.request.{BreakpointRequest, EventRequest, EventRequestManager, ExceptionRequest}
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer

trait PauseSupport { self: NashornDebuggerHost with Logging =>
  import scala.collection.JavaConverters._
  val ScriptClassPrefix = "jdk.nashorn.internal.scripts.Script"

  private val exceptionRequests = ListBuffer[ExceptionRequest]()

  // Breakpoints created to pause at the next executing statement.
  private val oneOffBreakpoints = ListBuffer[BreakpointRequest]()

  override def pauseOnExceptions(pauseType: ExceptionPauseType): Unit = {
    val erm = virtualMachine.eventRequestManager()

    // Clear all first, simpler than trying to keep in sync
    erm.deleteEventRequests(exceptionRequests.asJava)
    exceptionRequests.clear()

    val pauseOnCaught = pauseType == ExceptionPauseType.Caught || pauseType == ExceptionPauseType.All
    // Note that uncaught is currently untested since our test setup doesn't really allow it.
    val pauseOnUncaught = pauseType == ExceptionPauseType.Uncaught || pauseType == ExceptionPauseType.All

    if (pauseOnCaught || pauseOnUncaught) {
      log.info(s"Will pause on exceptions (caught=$pauseOnCaught, uncaught=$pauseOnUncaught)")
      val request = erm.createExceptionRequest(null, pauseOnCaught, pauseOnUncaught)
      request.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD) // TODO: Duplicate code
      request.setEnabled(true)
      exceptionRequests += request
    } else {
      log.info("Won't pause on exceptions")
    }
  }

  override def resume(): Unit = {
    resumeWhenPaused()
  }

  private def removeOneOffBreakpoints(): Unit = {
    val erm = virtualMachine.eventRequestManager()
    erm.deleteEventRequests(oneOffBreakpoints.asJava)
    oneOffBreakpoints.clear()
  }

  private def setOneOffBreakpointsForScriptFrames(scriptFrames: List[StackFrame]) = {
    def handler(e: Event): Unit = removeOneOffBreakpoints()
    val erm = virtualMachine.eventRequestManager()
    scriptFrames.foreach(sf => {
      val startLine = sf.location().lineNumber()
      val locationsForBreakpoints = sf.location().method().allLineLocations().asScala.filter(_.lineNumber() >= startLine)
      locationsForBreakpoints.foreach { l =>
        val bp = erm.createBreakpointRequest(l)
        bp.addCountFilter(1)
        bp.setEnabled(true)
        beforeEventIsHandled(bp)(handler)
        oneOffBreakpoints += bp
      }
    })
  }

  private def setMethodEntryBreakpoint() = {
    val erm = virtualMachine.eventRequestManager()
    val request = erm.createMethodEntryRequest()
    // TODO: Lots of duplicate code here wrt how we create breakpoints.
    request.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    // TODO: Should StepRequestClassFilter be this string?? But then maybe it won't be possible to step over _to_
    // TODO: a debugger statement. Check if we have a test for that!
    request.addClassFilter(ScriptClassPrefix + "*")
    request.addCountFilter(1)
    request.setEnabled(true)
  }

  override def pauseAtNextStatement(): Unit = pausedData match {
    case Some(_) =>
      log.warn("Won't pause at next statement because we're already paused!")
    case None =>

      // Suspend the VM, otherwise we cannot get thread frames safely.
      virtualMachine.suspend()
      try {
        // Two approaches to pausing:
        //
        // 1. Check if we're executing inside a script function. Find threads where there's a script involved, then
        //    enable one-time breakpoints on all subsequent locations in the executing script method.
        //
        // 2. Set a method-entry breakpoint for script functions.
        //
        // We do one of these but not both.

        val relevantThreads = virtualMachine.allThreads().asScala.filterNot(isInfrastructureThread).filter(isRunningThread)
        val scriptFrames = relevantThreads.flatMap { thread =>
          thread.frames().asScala.find(_.location().declaringType().name().startsWith(ScriptClassPrefix))
        }.toList

        scriptFrames match {
          case Nil =>
            // No executing scripts => case 2
            setMethodEntryBreakpoint()
          case _ =>
            // Executing scripts => case 1
            setOneOffBreakpointsForScriptFrames(scriptFrames)
        }
      } finally virtualMachine.resume()
  }

  override def setSkipAllPauses(skip: Boolean): Unit = {
    disablePausingAltogether = skip
  }

}
