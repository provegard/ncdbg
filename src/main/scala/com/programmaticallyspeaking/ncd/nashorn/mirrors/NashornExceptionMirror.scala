package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{Invokers, Marshaller}
import com.sun.jdi.{ClassType, ObjectReference, ThreadReference}

class NashornExceptionClassMirror(clazz: ClassType)(implicit marshaller: Marshaller) {
  import Mirrors._
  private implicit val thread: ThreadReference = marshaller.thread
  private lazy val invoker = Invokers.shared.getStatic(clazz)

  def getScriptStackString(obj: ObjectReference): String = invoker.getScriptStackString(obj).asString
}

class NashornExceptionMirror(exception: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._
  private implicit val thread: ThreadReference = marshaller.thread
  private lazy val invoker = Invokers.shared.getDynamic(exception)

  lazy val lineNumber: Int = invoker.getLineNumber().asNumber(throw new IllegalStateException("NashornException.getLineNumber")).intValue()
  lazy val columnNumber: Int = invoker.getColumnNumber().asNumber(throw new IllegalStateException("NashornException.getColumnNumber")).intValue()
  lazy val fileName: String = invoker.getFileName().asString
}
