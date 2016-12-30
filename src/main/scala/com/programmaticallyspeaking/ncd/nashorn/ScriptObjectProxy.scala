package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{LazyNode, SimpleValue, ValueNode}
import com.sun.jdi.{ArrayReference, ObjectReference, ThreadReference}

class ScriptObjectProxy(val scriptObject: ObjectReference, thread: ThreadReference, marshaller: Marshaller) {
  import scala.collection.JavaConversions._
  lazy val invoker = new DynamicInvoker(thread, scriptObject)

  lazy val className = {
    marshaller.marshal(invoker.getClassName()) match {
      case SimpleValue(s: String) => s
      case other => throw new IllegalStateException("Expected getClassName to be marshalled to a SimpleValue(String), but got: " + other)
    }
  }

  lazy val isArray = {
    marshaller.marshal(invoker.isArray()) match {
      case SimpleValue(b: Boolean) => b
      case other => throw new IllegalStateException("Expected isArray to be marshalled to a SimpleValue(Boolean), but got: " + other)
    }
  }

  def isFunction = className == "Function"
  def isError = className == "Error"
  def isDate = className == "Date"

  def entrySet(): Map[ValueNode, LazyNode] = {
    val entrySet = invoker.entrySet().asInstanceOf[ObjectReference]
    val entrySetInvoker = new DynamicInvoker(thread, entrySet)
    val array = entrySetInvoker.toArray().asInstanceOf[ArrayReference]
    array.getValues.map { v =>
      val entry = v.asInstanceOf[ObjectReference]
      val entryInvoker = new DynamicInvoker(thread, entry)
      val key = entryInvoker.getKey()
      val value = entryInvoker.getValue()
      marshaller.marshal(key) -> new LazyNode {
        override def resolve(): ValueNode = marshaller.marshal(value)
      }
    }.toMap
  }
}
