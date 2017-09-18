package com.programmaticallyspeaking.ncd.host

trait ScriptEvent

/**
  * Emitted when the remote VM has paused, typically on a breakpoint. The breakpoint can be set by the user, be a
  * 'debugger' statement, or be the result of a thrown exception when exception pausing is enabled.
  *
  * @param stackFrames the current script stack frames
  * @param breakpointId the ID of the breakpoint; for a 'debugger' statement or exception, the ID is not stable
  */
case class HitBreakpoint(stackFrames: Seq[StackFrame], breakpointId: String) extends ScriptEvent

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