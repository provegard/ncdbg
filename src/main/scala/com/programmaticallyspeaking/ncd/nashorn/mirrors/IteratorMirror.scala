package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Marshaller}
import com.sun.jdi.ObjectReference

import scala.language.implicitConversions
import scala.reflect.ClassTag

class IteratorMirror[T <: AnyRef : ClassTag](iteratorObject: ObjectReference)(implicit marshaller: Marshaller) extends Iterator[T] {
  import Mirrors._
  private lazy val invoker = new DynamicInvoker(marshaller.thread, iteratorObject)

  override def hasNext: Boolean = invoker.hasNext().asBool(false)

  override def next(): T = invoker.next().as[T].getOrElse(null.asInstanceOf[T])
}
