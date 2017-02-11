package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.host.ValueNode
import com.programmaticallyspeaking.ncd.nashorn.Marshaller
import com.sun.jdi.{ObjectReference, Value}

import scala.language.implicitConversions
import scala.reflect.ClassTag

object Mirrors {
  // TODO: Restrict! Should not auto convert to JDI classes...
  // Note: AnyRef because null => Int is not a good idea
  // TODO: Huh, Using AnyRef didn't prevent it from matching Int!! Why??
//  implicit def value2Any[R <: Any : ClassTag](value: Value)(implicit marshaller: Marshaller): R = {
//    assert(!classOf[Value].isAssignableFrom(implicitly[ClassTag[R]].runtimeClass), "Don't use implicit value-to-Any conversion for a JDI Value target.")
//    marshaller.marshalledAs[R](value)
//  }

  def asObjectReference(value: Value): Option[ObjectReference] = value match {
    case v if Marshaller.isUndefined(v) => None
    case o: ObjectReference => Some(o)
    case _ => None // TODO: or throw??
  }

  implicit def autoMarshal(value: Value)(implicit marshaller: Marshaller): ValueNode = marshaller.marshal(value)
}
