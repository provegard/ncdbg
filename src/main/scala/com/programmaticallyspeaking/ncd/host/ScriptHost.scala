package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.messaging.Observable

import scala.util.Try

case class Breakpoint(breakpointId: String, scriptId: String, lineNumberBase1: Int)

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

/**
  * Return type for [[ScriptHost]] methods that otherwise would return [[Unit]]. The point is to make the typed actor
  * implementation treat all methods as synchronous rather than treating the `Unit`-returning ones as fire-and-forget.
  * One alternative would be to let all methods return [[scala.concurrent.Future]], but it makes the trait very awkward
  * to implement.
  */
trait Done
object Done extends Done

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

  def removeBreakpointById(id: String): Done

  def resume(): Done

  /**
    * Resets the host - resumes if the host is paused and disables all breakpoints.
    */
  def reset(): Done

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

  def getBreakpointLineNumbers(scriptId: String, fromLineNumberBase1: Int, toLineNumberBase1: Option[Int]): Seq[Int]

  def step(stepType: StepType): Done

  /**
    * Tells the host to pause when it encounters a breakpoint.
    */
  def pauseOnBreakpoints(): Done

  /**
    * Tells the host to ignore breakpoints.
    */
  def ignoreBreakpoints(): Done

  def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor]

  def pauseOnExceptions(pauseType: ExceptionPauseType): Done
}
