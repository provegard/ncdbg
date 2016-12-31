package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{LazyNode, SimpleValue, ValueNode}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi.{ArrayReference, ObjectReference, ThreadReference}

/**
  * Wraps a [[ScriptObjectMirror]] instance to do marshalling and "unpacking" of data.
  *
  * @param mirror the mirror instance that is responsible for `ScriptObject` interaction
  * @param thread the thread on which to execute methods (for further unpacking of data)
  * @param marshaller marshaller for marshalling JDI `Value` into [[ValueNode]]
  */
class ScriptObjectProxy(val mirror: ScriptObjectMirror, thread: ThreadReference, marshaller: Marshaller) {
  import scala.collection.JavaConversions._

  val scriptObject = mirror.scriptObject

  lazy val className = {
    marshaller.marshal(mirror.getClassName) match {
      case SimpleValue(s: String) => s
      case other => throw new IllegalStateException("Expected getClassName to be marshalled to a SimpleValue(String), but got: " + other)
    }
  }

  lazy val isArray = {
    marshaller.marshal(mirror.isArray) match {
      case SimpleValue(b: Boolean) => b
      case other => throw new IllegalStateException("Expected isArray to be marshalled to a SimpleValue(Boolean), but got: " + other)
    }
  }

  def isFunction = className == "Function"
  def isError = className == "Error"
  def isDate = className == "Date"

  def entrySet(): Map[ValueNode, LazyNode] = {
    val entrySet = mirror.entrySet()
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
