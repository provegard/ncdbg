package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.nashorn.TypeConstants.ScriptRuntime_DEBUGGER
import com.sun.jdi.ClassType
import com.sun.jdi.request.EventRequest
import org.slf4s.Logging

object Breakpoints extends Logging {
  import scala.collection.JavaConverters._
  import com.programmaticallyspeaking.ncd.infra.BetterOption._

  private def enableBreakingAt(theType: ClassType, methodName: String, statementName: String): Unit = {
    val typeName = theType.name()
    val methodLoc = for {
      theMethod <- theType.methodsByName(methodName).asScala.headOption.toEither(s"$typeName.$methodName method not found")
      location <- theMethod.allLineLocations().asScala.headOption.toEither(s"no line location found in $typeName.$methodName")
    } yield location

    methodLoc match {
      case Right(location) =>
        log.info(s"Enabling automatic breaking at JavaScript '$statementName' statements")
        // TODO: BreakableLocation also does this. Reuse code!
        val br = theType.virtualMachine().eventRequestManager().createBreakpointRequest(location)
        br.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
        br.setEnabled(true)
      case Left(msg) =>
        log.warn(s"Won't be able to break at JavaScript '$statementName' statements because $msg")
    }
  }

  def enableBreakingAtDebuggerStatement(ct: ClassType): Unit =
    enableBreakingAt(ct, ScriptRuntime_DEBUGGER, "debugger")

}
