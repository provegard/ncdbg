package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.host.ValueNode
import com.programmaticallyspeaking.ncd.nashorn.Marshaller
import com.sun.jdi.{ObjectReference, Value}

import scala.language.implicitConversions

object Mirrors {
  def asObjectReference(value: Value): Option[ObjectReference] = value match {
    case v if Marshaller.isUndefined(v) => None
    case o: ObjectReference => Some(o)
    case _ => None // TODO: or throw??
  }

  implicit def autoMarshal(value: Value)(implicit marshaller: Marshaller): ValueNode = marshaller.marshal(value)
}
