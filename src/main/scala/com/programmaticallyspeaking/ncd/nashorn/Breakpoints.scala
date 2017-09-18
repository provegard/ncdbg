package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.{ClassType, Location}
import com.sun.jdi.request.{BreakpointRequest, EventRequest}
import org.slf4s.Logging

object Breakpoints extends Logging {
  import scala.collection.JavaConverters._
  import com.programmaticallyspeaking.ncd.infra.BetterOption._
  import com.programmaticallyspeaking.ncd.nashorn.TypeConstants._

  private def breakLocation(theType: ClassType, methodName: String): Either[String, Location] = {
    val typeName = theType.name()
    for {
      theMethod <- theType.methodsByName(methodName).asScala.headOption.toEither(s"$typeName.$methodName method not found")
      location <- theMethod.allLineLocations().asScala.headOption.toEither(s"no line location found in $typeName.$methodName")
    } yield location
  }

  // TODO: BreakableLocation also does this. Reuse code!
  private def createBreakpointAt(location: Location) = {
    val br = location.virtualMachine().eventRequestManager().createBreakpointRequest(location)
    br.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    br.setEnabled(true)
    br
  }

  private def enableBreakingAt(theType: ClassType, methodName: String, statementName: String): Unit = {
    breakLocation(theType, methodName) match {
      case Right(location) =>
        log.info(s"Enabling automatic breaking at JavaScript '$statementName' statements")
        createBreakpointAt(location)
      case Left(msg) =>
        log.warn(s"Won't be able to break at JavaScript '$statementName' statements because $msg")
    }
  }

  def enableBreakingAtDebuggerStatement(ct: ClassType): Unit =
    enableBreakingAt(ct, ScriptRuntime_DEBUGGER, "debugger")

  def enableBreakingAtGlobalPrint(ct: ClassType)(handler: Seq[BreakpointRequest] => Unit): Unit = {
    // Global.print exists, but what is it used for??
    (for {
      l1 <- breakLocation(ct, Global_print)
      l2 <- breakLocation(ct, Global_println)
    } yield Seq(l1, l2)) match {
      case Right(locs) =>
        handler(locs.map(createBreakpointAt))
      case Left(reason) =>
        log.warn(s"Won't be able to capture Nashorn print output because $reason")
        handler(Seq.empty)
    }
  }
}
