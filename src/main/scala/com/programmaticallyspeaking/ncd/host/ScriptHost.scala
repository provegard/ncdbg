package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.messaging.Observable

import scala.util.Try

case class Breakpoint(breakpointId: String, scriptId: String, lineNumberBase1: Int)

trait StackFrame {
  val id: String
  val thisObj: ValueNode
  val scopeObj: Option[ValueNode]

  /**
    * Artificially constructed object node to hold local variables.
    */
  val locals: ObjectNode

  val breakpoint: Breakpoint
  val functionDetails: FunctionDetails
}

/**
  * Contains data about a function referred to by a stack frame.
  *
  * @param name the function name
  */
case class FunctionDetails(name: String)

trait StepType
case object StepInto extends StepType
case object StepOver extends StepType
case object StepOut extends StepType

trait ScriptHost {
  def evaluateOnStackFrame(stackFrameId: String, expression: String): Try[ValueNode]

  def removeBreakpointById(id: String): Unit

  def resume(): Unit

  /**
    * Resets the host - resumes if the host is paused and disables all breakpoints.
    */
  def reset(): Unit

  def scriptById(id: String): Option[Script]

//  def scriptByUri(uri: String): Option[Script]

  def events: Observable[ScriptEvent]

  def scripts: Seq[Script]

  /**
    * Sets a breakpoint in the given script at the given line.
    *
    * @param scriptUri the URI of the script
    * @param lineNumberBase1 the 1-based line number in the script where the breakpoint should be set
    * @return a structure describing the breakpoint that was set
    */
  def setBreakpoint(scriptUri: String, lineNumberBase1: Int): Breakpoint

  def objectRegistry: ObjectRegistry

  def step(stepType: StepType): Unit
}
