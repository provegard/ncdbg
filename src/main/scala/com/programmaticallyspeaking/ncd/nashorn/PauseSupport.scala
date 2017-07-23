package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ExceptionPauseType
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{StepRequestClassFilter, isInfrastructureThread, isRunningThread}
import com.sun.jdi.{Location, Method, StackFrame}
import com.sun.jdi.event.Event
import com.sun.jdi.request.{BreakpointRequest, EventRequest, EventRequestManager, ExceptionRequest}
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer
import scala.util.Try

trait PauseSupport { self: NashornDebuggerHost with Logging =>
  import scala.collection.JavaConverters._
  val ScriptClassPrefix = "jdk.nashorn.internal.scripts.Script"

  private val exceptionRequests = ListBuffer[ExceptionRequest]()

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

  // allLineLocations may throw AbsentInformationException
  private def lineLocations(m: Method): Seq[Location] = Try(m.allLineLocations().asScala).getOrElse(Seq.empty)

  private def locationsForStackFrame(sf: StackFrame): Seq[Location] = {
    val startLine = sf.location().lineNumber()
    lineLocations(sf.location().method()).filter(_.lineNumber() > startLine)
  }

  private def setOneOffBreakpoint(location: Location) = {
    val erm = virtualMachine.eventRequestManager()
    val bp = erm.createBreakpointRequest(location)
    bp.addCountFilter(1)
    bp.setEnabled(true)
    bp
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
    request
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
        // We do both of these to ensure that the script is paused.

        // We're interested in all non-infrastructure threads.
        val relevantThreads = virtualMachine.allThreads().asScala.filterNot(isInfrastructureThread)

        // Get possible locations to break on. For each thread, find the first stack frame with non-empty locations
        // and get them (the locations).
        val locationsToSetBreakpointsOn = relevantThreads.flatMap { thread =>
          // Create a view so that we can map and find lazily
          val viewOfFrames = thread.frames().asScala.view
          viewOfFrames.map(locationsForStackFrame).find(_.nonEmpty).getOrElse(Seq.empty)
        }

        log.info("Will pause at next script statement")
        log.debug(s"Creating ${locationsToSetBreakpointsOn.size} one-off breakpoint request(s) and a method-entry request for pausing.")

        // Create both a method-entry request and breakpoint requests.
        val methodEntryRequest = setMethodEntryBreakpoint()
        val breakpoints = locationsToSetBreakpointsOn.map(setOneOffBreakpoint)
        val allRequests = breakpoints :+ methodEntryRequest

        // When we observe an event for a request, clear all the requests (of both types).
        def eventHandler(ev: Event) = {
          val erm = virtualMachine.eventRequestManager()
          erm.deleteEventRequests(allRequests.asJava)
        }

        // Associate the handler with each request
        allRequests.foreach(beforeEventIsHandled(_)(eventHandler))
      } finally virtualMachine.resume()
  }

  override def setSkipAllPauses(skip: Boolean): Unit = {
    disablePausingAltogether = skip
  }

}
