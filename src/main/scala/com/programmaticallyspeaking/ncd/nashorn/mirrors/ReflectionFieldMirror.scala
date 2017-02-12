package com.programmaticallyspeaking.ncd.nashorn.mirrors

import java.lang.reflect.Modifier

import com.programmaticallyspeaking.ncd.host.ValueNode
import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Marshaller}
import com.sun.jdi.ObjectReference

class ReflectionFieldMirror(field: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._

  private lazy val invoker = new DynamicInvoker(marshaller.thread, field)

  lazy val name = invoker.getName().asString

  def isAccessible = invoker.isAccessible().asBool(false)

  def setAccessible(b: Boolean): Unit = invoker.setAccessible(b)

  def get(obj: ObjectReference): ValueNode = invoker.get(obj)

  lazy val modifiers = invoker.getModifiers().asNumber(0).intValue()

  lazy val isFinal = Modifier.isFinal(modifiers)
}
