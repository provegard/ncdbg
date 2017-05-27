package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{Invokers, Marshaller}
import com.sun.jdi.{ObjectReference, ThreadReference}

class JSObjectMirror(val jsObject: ObjectReference)(implicit marshaller: Marshaller) {
  import JSObjectMirror._
  import Mirrors._

  private implicit val thread: ThreadReference = marshaller.thread
  private lazy val invoker = Invokers.shared.getDynamic(jsObject)

  lazy val className: String = invoker.getClassName().asString

  lazy val isArray: Boolean = invoker.isArray().asBool(false) || className == "Array"
  lazy val isFunction: Boolean = invoker.isFunction().asBool(false) || className == "Function"

  def getString(key: String): String = invoker.applyDynamic(getMemberSignature)(key).asString
  def getInt(key: String, defaultValue: Int): Int = invoker.applyDynamic(getMemberSignature)(key).asNumber(defaultValue).intValue()
}

object JSObjectMirror {
  val getMemberSignature = "getMember(Ljava/lang/String;)Ljava/lang/Object;"
}