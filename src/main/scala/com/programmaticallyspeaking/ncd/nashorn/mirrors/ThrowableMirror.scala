package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{Invokers, Marshaller}
import com.sun.jdi.{ArrayReference, ObjectReference, ThreadReference}

class ThrowableMirror(throwable: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._

  import scala.collection.JavaConverters._
  private implicit val thread: ThreadReference = marshaller.thread
  private lazy val invoker = Invokers.shared.getDynamic(throwable)

  lazy val stackTrace: Seq[StackTraceElementMirror] = invoker.getStackTrace() match {
    case arr: ArrayReference =>
      arr.getValues.asScala.collect { case o: ObjectReference => o }.map(new StackTraceElementMirror(_))

    case other => throw new IllegalStateException("getStackTrace didn't return an array: " + other)
  }

  lazy val message: String = invoker.getMessage().asString
}

class StackTraceElementMirror(stackTraceElement: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._
  private implicit val thread: ThreadReference = marshaller.thread
  private lazy val invoker = Invokers.shared.getDynamic(stackTraceElement)

  lazy val lineNumber: Int = invoker.getLineNumber().asNumber(throw new IllegalStateException("No getLineNumber method on StackTraceElement??")).intValue()
  lazy val fileName: String = invoker.getFileName().asString

  lazy val actualToString: String = invoker.applyDynamic("toString")().asString
}