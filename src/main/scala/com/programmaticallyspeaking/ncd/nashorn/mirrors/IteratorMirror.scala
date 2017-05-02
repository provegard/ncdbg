package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{Invokers, Marshaller}
import com.sun.jdi.{ObjectReference, ThreadReference}

import scala.language.implicitConversions
import scala.reflect.ClassTag

class IteratorMirror[T <: AnyRef : ClassTag](iteratorObject: ObjectReference)(implicit marshaller: Marshaller) extends Iterator[T] {
  import Mirrors._
  private implicit val thread: ThreadReference = marshaller.thread
  private lazy val invoker = Invokers.shared.getDynamic(iteratorObject)

  override def hasNext: Boolean = invoker.hasNext().asBool(false)

  override def next(): T = invoker.next().as[T].getOrElse(null.asInstanceOf[T])
}
