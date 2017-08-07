package com.programmaticallyspeaking.ncd.host

trait ScriptEvent

case class HitBreakpoint(stackFrames: Seq[StackFrame], breakpointId: String) extends ScriptEvent

case object Resumed extends ScriptEvent

case class ScriptAdded(script: Script) extends ScriptEvent

case class UncaughtError(error: ErrorValue) extends ScriptEvent