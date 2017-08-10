package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.messaging.Observable

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

case class ScriptLocation(lineNumber1Based: Int, columnNumber1Based: Option[Int]) {
  override def toString: String = lineNumber1Based + columnNumber1Based.map(c => ":" + c).getOrElse("")
}

case class Breakpoint(breakpointId: String, scriptId: String, scriptURL: Option[ScriptURL], locations: Seq[ScriptLocation]) {
  require(locations.nonEmpty, "A Breakpoint must have locations")
  def location: ScriptLocation = locations.head //TODO: for tests, remove
}

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
  val functionDetails: FunctionDetails
  val scriptId: String
  val scriptURL: ScriptURL
  val location: ScriptLocation
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
    * Indicates that all pauses should be skipped or not skipped. This flag is applied "on top" of pausing on
    * breakpoints and exceptions. When the flag is reset to false, breakpoint and exception pausing happens as
    * configured earlier.
    *
    * @param skip true to skip all pauses
    */
  def setSkipAllPauses(skip: Boolean): Unit

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

  /**
    * Pauses script execution at the next statement.
    */
  def pauseAtNextStatement(): Unit

  def resume(): Unit

  /**
    * Resets the host - resumes if the host is paused and disables all breakpoints.
    */
  def reset(): Unit

  def findScript(id: ScriptIdentity): Option[Script]

  def events: Observable[ScriptEvent]

  def scripts: Seq[Script]

  /**
    * Sets a breakpoint in the given script at the given line.
    *
    * @param id the script identifier
    * @param location the location of the breakpoint; if the column number isn't set, the returned breakpoint may
    *                 refer to multiple locations.
    * @param condition optional condition to use for the breakpoint
    * @return a structure describing the breakpoint that was set
    */
  def setBreakpoint(id: ScriptIdentity, location: ScriptLocation, condition: Option[String]): Option[Breakpoint]

  def getBreakpointLocations(id: ScriptIdentity, from: ScriptLocation, to: Option[ScriptLocation]): Seq[ScriptLocation]

  def step(stepType: StepType): Unit

  /**
    * Tells the host to pause when it encounters a breakpoint.
    */
  def pauseOnBreakpoints(): Unit

  /**
    * Tells the host to ignore breakpoints.
    */
  def ignoreBreakpoints(): Unit

  /**
    * Returns a list of named object property descriptors, including symbol properties.
    *
    * @param objectId the ID of the object whose properties to get
    * @param onlyOwn true if only own properties should be extracted
    * @param onlyAccessors true if only accessors should be extracted
    * @return a list of named object property descriptors
    */
  def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)]

  def pauseOnExceptions(pauseType: ExceptionPauseType): Unit

  /**
    * Restart the frame with the given ID. This operation can only be done when the VM is paused. The VM will remain
    * paused afterwards.
    *
    * @param stackFrameId the ID of the stack frame to restart
    * @return the stack frames after restarting
    */
  def restartStackFrame(stackFrameId: String): Seq[StackFrame]

  /**
    * Start profiling with the given sampling interval. Since there's some overhead when collecting stack frames via
    * JDI, it's not guaranteed that the resulting sampling interval will equal the requested. There can only be one
    * ongoing profiling "session" at a time.
    *
    * @param samplingInterval requested sampling interval
    */
  def startProfiling(samplingInterval: FiniteDuration): Unit

  /**
    * Stops the current profiling "session" and returns profiling data.
    *
    * @return profiling data
    */
  def stopProfiling(): ProfilingData
}

/**
  * Contains profiling data.
  *
  * @param samples a list of samples
  * @param startNanos the start time obtained using [[System.nanoTime()]]
  * @param stopNanos the stop time obtained using [[System.nanoTime()]]
  */
case class ProfilingData(samples: Seq[Sample], startNanos: Long, stopNanos: Long)

/**
  * Contains an individual profiling sample.
  *
  * @param nanoTime sample time obtained using [[System.nanoTime()]]
  * @param stackFrames a list of script stack frames, possibly empty
  * @param sampleType type of sample
  */
case class Sample(nanoTime: Long, stackFrames: Seq[StackFrame], sampleType: SampleType.SampleType)

object SampleType extends Enumeration {
  type SampleType = Value
  val Script, Java, Idle = Value
}