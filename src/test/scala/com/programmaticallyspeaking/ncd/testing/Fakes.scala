package com.programmaticallyspeaking.ncd.testing

import java.io.File
import java.net.URL

import com.programmaticallyspeaking.ncd.chrome.net.FilePublisher
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.{Observable, SerializedSubject}

import scala.util.Try

object FakeFilePublisher extends FilePublisher {
  override def publish(file: File): URL = new URL("http://localhost/no/such/file")
}

object FakeScriptHost extends ScriptHost {
  val eventSubject = new SerializedSubject[ScriptEvent]

  override def evaluateOnStackFrame(stackFrameId: String, expression: String, namedObjects: Map[String, ObjectId]): Try[ValueNode] = Try(notImpl)
  override def removeBreakpointById(id: String): Unit = notImpl
  override def resume(): Unit = notImpl
  override def reset(): Unit = notImpl
  override def scriptById(id: String): Option[Script] = None
  override def events: Observable[ScriptEvent] = eventSubject
  override def scripts: Seq[Script] = Seq.empty
  override def setBreakpoint(scriptUri: String, location: ScriptLocation, condition: Option[String]): Option[Breakpoint] = notImpl
  override def getBreakpointLocations(scriptId: String, from: ScriptLocation, to: Option[ScriptLocation]): Seq[ScriptLocation] = Seq.empty
  override def step(stepType: StepType): Unit = notImpl
  override def pauseOnBreakpoints(): Unit = notImpl
  override def ignoreBreakpoints(): Unit = notImpl
  override def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, types.ObjectPropertyDescriptor] = Map.empty
  override def pauseOnExceptions(pauseType: ExceptionPauseType): Unit = notImpl
  
  private def notImpl[R]: R = throw new UnsupportedOperationException("FakeScriptHost is not complete")

  override def restartStackFrame(stackFrameId: String): Seq[StackFrame] = notImpl
}
