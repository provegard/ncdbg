package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.host.ValueNode
import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Marshaller}
import com.sun.jdi.ObjectReference

class JSObjectMirror(val jsObject: ObjectReference)(implicit marshaller: Marshaller) {
  import JSObjectMirror._
  import Mirrors._

  private lazy val invoker = new DynamicInvoker(marshaller.thread, jsObject)

  lazy val className: String = invoker.getClassName().asString

  lazy val isArray: Boolean = invoker.isArray().asBool(false) || className == "Array"
  lazy val isFunction: Boolean = invoker.isFunction().asBool(false) || className == "Function"

  def keySet(): Set[String] = {
    val theSet = invoker.keySet().asInstanceOf[ObjectReference]
    val setInvoker = new DynamicInvoker(marshaller.thread, theSet)
    val iterator = new IteratorMirror[String](setInvoker.iterator().asInstanceOf[ObjectReference])
    iterator.toSet
  }

  def getString(key: String): String = invoker.applyDynamic(getMemberSignature)(key).asString
  def getInt(key: String, defaultValue: Int): Int = invoker.applyDynamic(getMemberSignature)(key).asInt(defaultValue)

  def getUnknown(key: String): ValueNode = invoker.applyDynamic(getMemberSignature)(key)

  def getSlot(index: Int): ValueNode = invoker.applyDynamic(getSlotSignature)(index)
}

object JSObjectMirror {
  val getMemberSignature = "getMember(Ljava/lang/String;)Ljava/lang/Object;"
  val getSlotSignature = "getSlot(I)Ljava/lang/Object;"
}