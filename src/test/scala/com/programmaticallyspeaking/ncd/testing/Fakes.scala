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
  override def removeBreakpointById(id: String): Done = notImpl
  override def resume(): Done = notImpl
  override def reset(): Done = notImpl
  override def scriptById(id: String): Option[Script] = None
  override def events: Observable[ScriptEvent] = eventSubject
  override def scripts: Seq[Script] = Seq.empty
  override def setBreakpoint(scriptUri: String, location: ScriptLocation, condition: Option[String]): Option[Breakpoint] = notImpl
  override def getBreakpointLocations(scriptId: String, from: ScriptLocation, to: Option[ScriptLocation]): Seq[ScriptLocation] = Seq.empty
  override def step(stepType: StepType): Done = notImpl
  override def pauseOnBreakpoints(): Done = notImpl
  override def ignoreBreakpoints(): Done = notImpl
  override def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, types.ObjectPropertyDescriptor] = Map.empty
  override def pauseOnExceptions(pauseType: ExceptionPauseType): Done = notImpl
  
  private def notImpl[R]: R = throw new UnsupportedOperationException("FakeScriptHost is not complete")
}
