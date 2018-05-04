package com.programmaticallyspeaking.ncd.testing

import java.io.File
import java.net.URL

import com.programmaticallyspeaking.ncd.chrome.net.FilePublisher
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.{Observable, SerializedSubject}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

object FakeFilePublisher extends FilePublisher {
  override def publish(file: File): URL = new URL("http://localhost/no/such/file")
}

object FakeScriptHost extends ScriptHost {
  val eventSubject = new SerializedSubject[ScriptEvent]

  override def evaluateOnStackFrame(stackFrameId: String, expression: String): Try[ValueNode] = Try(notImpl)
  override def removeBreakpointById(id: String): Unit = notImpl
  override def resume(): Unit = notImpl
  override def reset(): Unit = notImpl
  override def findScript(id: ScriptIdentity): Option[Script] = None
  override def events: Observable[ScriptEvent] = eventSubject
  override def scripts: Seq[Script] = Seq.empty
  override def setBreakpoint(id: ScriptIdentity, location: ScriptLocation, options: BreakpointOptions): Breakpoint = notImpl
  override def getBreakpointLocations(id: ScriptIdentity, from: ScriptLocation, to: Option[ScriptLocation]): Seq[ScriptLocation] = Seq.empty
  override def step(stepType: StepType): Unit = notImpl
  override def pauseOnBreakpoints(): Unit = notImpl
  override def ignoreBreakpoints(): Unit = notImpl
  override def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, types.ObjectPropertyDescriptor)] = Seq.empty
  override def pauseOnExceptions(pauseType: ExceptionPauseType): Unit = notImpl
  
  private def notImpl: Nothing = throw new UnsupportedOperationException("FakeScriptHost is not complete")

  override def restartStackFrame(stackFrameId: String): Seq[StackFrame] = notImpl

  override def startProfiling(samplingInterval: FiniteDuration): Unit = notImpl
  override def stopProfiling(): ProfilingData = notImpl

  override def pauseAtNextStatement(): Unit = notImpl

  override def setSkipAllPauses(skip: Boolean): Unit = notImpl

  override def compileScript(script: String, url: String, persist: Boolean): Future[Option[Script]] = Future.failed(notImpl)

  override def runCompiledScript(scriptId: String): Try[ValueNode] = notImpl

  override def warnings: Seq[String] = Seq.empty

  override def callFunctionOn(stackFrameId: String, thisObject: Option[ObjectId], functionDeclaration: String, arguments: Seq[ObjectId]): Try[ValueNode] = Try(notImpl)
}
