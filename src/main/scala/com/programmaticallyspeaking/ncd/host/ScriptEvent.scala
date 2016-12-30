package com.programmaticallyspeaking.ncd.host

trait ScriptEvent

case class HitBreakpoint(stackFrames: Seq[StackFrame]) extends ScriptEvent

case object Resumed extends ScriptEvent