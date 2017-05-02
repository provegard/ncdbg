package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.host.ValueNode
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.infra.StringUtils.isUnsignedInt
import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Invokers, Marshaller, PropertyHolder}
import com.sun.jdi.{ObjectReference, ThreadReference}

class JSObjectMirror(val jsObject: ObjectReference)(implicit marshaller: Marshaller) extends PropertyHolder {
  import JSObjectMirror._
  import Mirrors._

  private implicit val thread: ThreadReference = marshaller.thread
  private lazy val invoker = Invokers.shared.getDynamic(jsObject)

  lazy val className: String = invoker.getClassName().asString

  lazy val isArray: Boolean = invoker.isArray().asBool(false) || className == "Array"
  lazy val isFunction: Boolean = invoker.isFunction().asBool(false) || className == "Function"

  def keySet(): Set[String] = {
    val theSet = invoker.keySet().asInstanceOf[ObjectReference]
    val setInvoker = Invokers.shared.getDynamic(theSet)
    val iterator = new IteratorMirror[String](setInvoker.iterator().asInstanceOf[ObjectReference])
    iterator.toSet
  }

  def getString(key: String): String = invoker.applyDynamic(getMemberSignature)(key).asString
  def getInt(key: String, defaultValue: Int): Int = invoker.applyDynamic(getMemberSignature)(key).asNumber(defaultValue).intValue()

  def getMember(key: String): ValueNode = invoker.applyDynamic(getMemberSignature)(key)

  def getSlot(index: Int): ValueNode = invoker.applyDynamic(getSlotSignature)(index)

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    if (onlyAccessors) return Map.empty // JSObject cannot have accessor properties

    // For an array, keySet should return indices + "length", and then we get use getSlot.
    keySet().map { prop =>
      val theValue =
        if (isArray && isUnsignedInt(prop)) getSlot(prop.toInt) else getMember(prop)

      // Note: A ValueNode shouldn't be null/undefined, so use Some(...) rather than Option(...) for the value
      prop -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
        isWritable = true, isOwn = true, Some(theValue), None, None)
    }.toMap
  }
}

object JSObjectMirror {
  val getMemberSignature = "getMember(Ljava/lang/String;)Ljava/lang/Object;"
  val getSlotSignature = "getSlot(I)Ljava/lang/Object;"
}