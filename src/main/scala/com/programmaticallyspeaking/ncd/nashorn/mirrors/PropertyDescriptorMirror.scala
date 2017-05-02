package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.host.ValueNode
import com.programmaticallyspeaking.ncd.nashorn.{Invokers, Marshaller}
import com.sun.jdi.{ObjectReference, ThreadReference}

class PropertyDescriptorMirror(propertyDescriptor: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._
  private implicit val thread: ThreadReference = marshaller.thread
  private val invoker = Invokers.shared.getDynamic(propertyDescriptor)

  lazy val isConfigurable: Boolean = invoker.isConfigurable().asBool(false)
  lazy val isEnumerable: Boolean = invoker.isEnumerable().asBool(false)
  lazy val isWritable: Boolean = invoker.isWritable().asBool(false)

  lazy val getType: Int = invoker.`type`().asNumber(-1).intValue()

  // Not relevant to return these as real functions, though it causes the mirror interface to be a bit weird
  lazy val getValue: ValueNode = marshaller.marshal(invoker.getValue())
  lazy val getGetter: ValueNode = marshaller.marshal(invoker.getGetter())
  lazy val getSetter: ValueNode = marshaller.marshal(invoker.getSetter())
}
