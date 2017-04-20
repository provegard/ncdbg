package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Marshaller}
import com.sun.jdi.{ArrayReference, ObjectReference}

class ClassMirror(clazz: ObjectReference)(implicit marshaller: Marshaller) {
  import scala.collection.JavaConverters._
  import Mirrors._

  private val classInvoker = new DynamicInvoker(marshaller.thread, clazz)

  lazy val declaredFields: Seq[ReflectionFieldMirror] = classInvoker.getDeclaredFields() match {
    case arr: ArrayReference =>
      arr.getValues.asScala.collect { case ref: ObjectReference => ref }.map(ref => new ReflectionFieldMirror(ref))

    case other => throw new IllegalStateException("Unexpected getDeclaredFields return value: " + other)
  }

  lazy val publicMethods: Seq[ReflectionMethodMirror] = classInvoker.getMethods() match {
    case arr: ArrayReference =>
      arr.getValues.asScala.collect { case ref: ObjectReference => ref }.map(ref => new ReflectionMethodMirror(ref))

    case other => throw new IllegalStateException("Unexpected getMethods return value: " + other)
  }

  lazy val name: String = classInvoker.getName().asString
}
