package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Marshaller}
import com.sun.jdi._

import scala.language.implicitConversions

/**
  * Mirror for `jdk.nashorn.internal.runtime.ScriptObject`.
  *
  * @param scriptObject the `ScriptObject` instance to interact with
  */
class ScriptObjectMirror(val scriptObject: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._
  import ScriptObjectMirror._

  import scala.collection.JavaConverters._

  protected lazy val invoker = new DynamicInvoker(marshaller.thread, scriptObject)

  lazy val className: String = invoker.getClassName().asString

  lazy val isArray: Boolean = invoker.isArray().asBool(false)

  def propertyIterator(): Iterator[String] = new IteratorMirror[String](invoker.propertyIterator().asInstanceOf[ObjectReference]) // Iterator<String>

  def getOwnKeys(all: Boolean): Array[String] = invoker.getOwnKeys(all) match {
    case arr: ArrayReference => arr.getValues.asScala.map(_.asString).toArray
    case other => throw new IllegalStateException("Expected ScriptObject.getOwnKeys to return an array")
  }

  def getOwnPropertyDescriptor(property: String): Option[PropertyDescriptorMirror] =
    asObjectReference(invoker.getOwnPropertyDescriptor(property)).map(o => new PropertyDescriptorMirror(o))

  def getPropertyDescriptor(property: String) =
    asObjectReference(invoker.getPropertyDescriptor(property)).map(o => new PropertyDescriptorMirror(o))

  def put(key: AnyRef, value: AnyRef, isStrict: Boolean): Unit =
    invoker.applyDynamic(putObjectObjectBoolSignature)(key, value, isStrict)

  def getString(key: AnyRef): String = invoker.applyDynamic(getObjectSignature)(key).asString
  def getInt(key: AnyRef, defaultValue: Int): Int = invoker.applyDynamic(getObjectSignature)(key).asNumber(defaultValue).intValue()

  def getRequiredInt(key: AnyRef): Int =
    invoker.applyDynamic(getObjectSignature)(key).asNumber(throw new IllegalStateException(s"Property $key doesn't have a number value")).intValue()

  def actualToString: String = invoker.applyDynamic("toString")().asString

  override def toString = "ScriptObjectMirror (maybe you meant actualToString?)"

  def asFunction = {
    assert(className == "Function", "asFunction can only be called for a function")
    new ScriptFunctionMirror(scriptObject)
  }
}

class ScriptFunctionMirror(scriptObject: ObjectReference)(implicit marshaller: Marshaller) extends ScriptObjectMirror(scriptObject) {
  import Mirrors._

  lazy val name: String = invoker.getName().asString
  lazy val source: String = invoker.toSource().asString
}

object ScriptObjectMirror {
  val putObjectObjectBoolSignature = "put(Ljava/lang/Object;Ljava/lang/Object;Z)Ljava/lang/Object;"
  val getObjectSignature = "get(Ljava/lang/Object;)Ljava/lang/Object;"
}