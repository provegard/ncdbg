package com.programmaticallyspeaking.ncd.host

trait ScriptEvent

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
  * @param data data associated with the breakpoint
  */
//TODO: A better name is needed, since it's not necessarily a breakpoint
case class HitBreakpoint(stackFrames: Seq[StackFrame], breakpointId: Option[String], reason: BreakpointReason, data: Option[ValueNode]) extends ScriptEvent

/**
  * Emitted when the remote VM resumes execution.
  */
case object Resumed extends ScriptEvent

/**
  * Emitted when a script has been discovered.
  *
  * @param script the script
  */
case class ScriptAdded(script: Script) extends ScriptEvent

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
case class PrintMessage(message: String) extends ScriptEvent