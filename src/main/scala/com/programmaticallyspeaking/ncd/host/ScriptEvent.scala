package com.programmaticallyspeaking.ncd.host

trait ScriptEvent {
  override def toString: String = {
    var name = getClass.getSimpleName
    // Remove trailing $ for objects
    if (name.endsWith("$")) name = name.substring(0, name.length - 1)
    val paramStr = toStringParams().map(e => e._1 + "=" + e._2).mkString(", ")
    s"$name($paramStr)"
  }

  protected def toStringParams(): Map[String, Any] = Map.empty
}

sealed trait BreakpointReason
object BreakpointReason {
  case object Breakpoint extends BreakpointReason
  case class Exception(data: Option[ErrorValue]) extends BreakpointReason
  case object Debugger extends BreakpointReason
  case object Step extends BreakpointReason
}

/**
  * Emitted when the remote VM has paused, typically on a breakpoint. The breakpoint can be set by the user, be a
  * 'debugger' statement, or be the result of a thrown exception when exception pausing is enabled.
  *
  * @param stackFrames the current script stack frames
  * @param breakpointId the ID of the breakpoint if an actual breakpoint was hit
  * @param reason reason for the breakpoint
  */
//TODO: A better name is needed, since it's not necessarily a breakpoint
case class HitBreakpoint(stackFrames: Seq[StackFrame], breakpointId: Option[String], reason: BreakpointReason) extends ScriptEvent {
  override protected def toStringParams(): Map[String, Any] = Map("breakpointId" -> breakpointId)
}

/**
  * Emitted when the remote VM resumes execution.
  */
case object Resumed extends ScriptEvent

/**
  * Emitted when a script has been discovered.
  *
  * @param script the script
  */
case class ScriptAdded(script: Script) extends ScriptEvent {
  override def toStringParams(): Map[String, Any] = Map("scriptId" -> script.id)
}

/**
  * Emitted when an uncaught error is detected, regardless of whether exception pausing is enabled or not.
  *
  * @param error the error
  */
case class UncaughtError(error: ErrorValue) extends ScriptEvent

/**
  * Emitted when the host considers initialization to be complete; currently when all needed reference types have been
  * identified.
  */
object InitialInitializationComplete extends ScriptEvent

/**
  * Emitted when a `print` or `println` statement (Nashorn extension) are encountered in script code.
  *
  * @param message the message passed to the print function
  */
case class PrintMessage(message: String) extends ScriptEvent {
  override def toStringParams(): Map[String, Any] = Map("message" -> message)
}

/**
  * Emitted when a new script is detected and there are breakpoints that match it.
  *
  * @param breakpointId the ID of the breakpoint
  * @param location the matched location
  */
case class BreakpointResolved(breakpointId: String, location: LocationInScript) extends ScriptEvent {
  override def toStringParams(): Map[String, Any] = Map("breakpointId" -> breakpointId)
}