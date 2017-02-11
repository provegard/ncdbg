package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{Marshaller, StaticInvoker}
import com.sun.jdi.{ClassType, ObjectReference}

class NashornExceptionMirror(clazz: ClassType)(implicit marshaller: Marshaller) {
  import Mirrors._
  private lazy val invoker = new StaticInvoker(marshaller.thread, clazz)

  def getScriptStackString(obj: ObjectReference): String = invoker.getScriptStackString(obj).asString
}
