package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ExceptionPauseType
import com.sun.jdi.request.{EventRequest, ExceptionRequest}
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer

trait PauseSupport { self: NashornDebuggerHost with Logging =>
  import scala.collection.JavaConverters._

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

}
