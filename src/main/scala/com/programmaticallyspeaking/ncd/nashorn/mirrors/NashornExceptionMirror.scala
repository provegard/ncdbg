package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Marshaller, StaticInvoker}
import com.sun.jdi.{ClassType, ObjectReference}

class NashornExceptionClassMirror(clazz: ClassType)(implicit marshaller: Marshaller) {
  import Mirrors._
  private lazy val invoker = new StaticInvoker(marshaller.thread, clazz)

  def getScriptStackString(obj: ObjectReference): String = invoker.getScriptStackString(obj).asString
}

class NashornExceptionMirror(exception: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._
  private lazy val invoker = new DynamicInvoker(marshaller.thread, exception)

  lazy val lineNumber: Int = invoker.getLineNumber().asNumber(throw new IllegalStateException("NashornException.getLineNumber")).intValue()
  lazy val columnNumber: Int = invoker.getColumnNumber().asNumber(throw new IllegalStateException("NashornException.getColumnNumber")).intValue()
  lazy val fileName: String = invoker.getFileName().asString
}
