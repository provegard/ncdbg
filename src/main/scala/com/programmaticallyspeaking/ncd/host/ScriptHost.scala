package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.messaging.Observable

import scala.util.Try

case class ScriptLocation(lineNumber1Based: Int, columnNumber1Based: Int) {
  override def toString: String = lineNumber1Based + ":" + columnNumber1Based
}

case class Breakpoint(breakpointId: String, scriptId: String, scriptURL: Option[ScriptURL], location: ScriptLocation)

sealed trait ScopeType
object ScopeType {
  object Local extends ScopeType
  object Global extends ScopeType
  object Closure extends ScopeType
  object With extends ScopeType
}

case class Scope(value: ValueNode, scopeType: ScopeType)

sealed trait ExceptionPauseType
object ExceptionPauseType {
  object None extends ExceptionPauseType
  object Caught extends ExceptionPauseType
  object Uncaught extends ExceptionPauseType
  object All extends ExceptionPauseType
}

trait StackFrame {
  val id: String
  val thisObj: ValueNode
  val scopeChain: Seq[Scope]
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
  /**
    * Evaluates an expression on a specific stack frame. Variables visible on that stack frame can be used in the
    * expression.
    *
    * This method is only valid to call when the host is paused.
    *
    * @param stackFrameId the ID of the stack frame to evaluate the expression on.
    * @param expression the expression to evaluate
    * @param namedObjects map of objects to expose as free variables
    * @return
    */
  def evaluateOnStackFrame(stackFrameId: String, expression: String, namedObjects: Map[String, ObjectId]): Try[ValueNode]

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
    * @param location the location in the script where the breakpoint should be set
    * @param condition optional condition to use for the breakpoint
    * @return a structure describing the breakpoint that was set
    */
  def setBreakpoint(scriptUri: String, location: ScriptLocation, condition: Option[String]): Option[Breakpoint]

  def getBreakpointLocations(scriptId: String, from: ScriptLocation, to: Option[ScriptLocation]): Seq[ScriptLocation]

  def step(stepType: StepType): Unit

  /**
    * Tells the host to pause when it encounters a breakpoint.
    */
  def pauseOnBreakpoints(): Unit

  /**
    * Tells the host to ignore breakpoints.
    */
  def ignoreBreakpoints(): Unit

  def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor]

  def pauseOnExceptions(pauseType: ExceptionPauseType): Unit

  /**
    * Restart the frame with the given ID. This operation can only be done when the VM is paused. The VM will remain
    * paused afterwards.
    *
    * @param stackFrameId the ID of the stack frame to restart
    * @return the stack frames after restarting
    */
  def restartStackFrame(stackFrameId: String): Seq[StackFrame]
}
