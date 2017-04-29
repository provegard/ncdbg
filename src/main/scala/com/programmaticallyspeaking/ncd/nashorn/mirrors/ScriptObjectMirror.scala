package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror.getObjectSignature
import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Marshaller, PropertyHolder}
import com.sun.jdi._

import scala.language.implicitConversions

/**
  * Mirror for `jdk.nashorn.internal.runtime.ScriptObject`.
  *
  * @param scriptObject the `ScriptObject` instance to interact with
  */
class ScriptObjectMirror(val scriptObject: ObjectReference)(implicit marshaller: Marshaller) extends PropertyHolder {
  import Mirrors._
  import ScriptObjectMirror._

  import scala.collection.JavaConverters._

  protected lazy val invoker = new DynamicInvoker(marshaller.thread, scriptObject)

  lazy val className: String = invoker.getClassName().asString

  lazy val isArray: Boolean = invoker.isArray().asBool(false)

  lazy val isRegularOrTypedArray: Boolean =
    isArray || className.endsWith("Array")

  def propertyIterator(): Iterator[String] = new IteratorMirror[String](invoker.propertyIterator().asInstanceOf[ObjectReference]) // Iterator<String>

  def getOwnKeys(all: Boolean): Array[String] = invoker.getOwnKeys(all) match {
    case arr: ArrayReference => arr.getValues.asScala.map(_.asString).toArray
    case other => throw new IllegalStateException("Expected ScriptObject.getOwnKeys to return an array, but got: " + other)
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

  def asArray = {
    assert(isArray, "asArray can only be called for an array")
    new ScriptArrayMirror(scriptObject)
  }

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    val propertyNames = if (onlyOwn) {
      // Get own properties, pass true to get non-enumerable ones as well (because they are relevant for debugging)
      getOwnKeys(true)
    } else {
      // Get all properties - this method walks the prototype chain
      propertyIterator().toArray
    }
    propertyNames.map { prop =>
      // Get either only the own descriptor or try both ways (own + proto). This is required for us to know
      // if the descriptor represents an own property.
      val ownDescriptor = getOwnPropertyDescriptor(prop)
      // TODO ugly, ugly.. make nicer
      val hasOwnDescriptor = ownDescriptor.isDefined

      val protoDescriptor = if (onlyOwn || hasOwnDescriptor) None else getPropertyDescriptor(prop)

      val descriptorToUse = protoDescriptor.orElse(ownDescriptor)
        .getOrElse(throw new IllegalStateException(s"No property descriptor for ${scriptObject.`type`().name()}.$prop"))

      // Read descriptor-generic information
      val theType = descriptorToUse.getType
      val isConfigurable = descriptorToUse.isConfigurable
      val isEnumerable = descriptorToUse.isEnumerable
      val isWritable = descriptorToUse.isWritable

      prop -> (theType match {
        case 0 =>
          // Generic
          ObjectPropertyDescriptor(PropertyDescriptorType.Generic, isConfigurable, isEnumerable, isWritable, hasOwnDescriptor,
            None, None, None)
        case 1 =>
          // Data, value is ok to use
          val theValue = descriptorToUse.getValue
          ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable, isEnumerable, isWritable, hasOwnDescriptor,
            Option(theValue), None, None)
        case 2 =>
          // Accessor, getter/setter are ok to use
          val getter = descriptorToUse.getGetter
          val setter = descriptorToUse.getSetter
          ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, isConfigurable, isEnumerable, isWritable, hasOwnDescriptor,
            None, Option(getter), Option(setter))
        case other => throw new IllegalArgumentException("Unknown property descriptor type: " + other)
      })
    }.filter(e => !onlyAccessors || e._2.descriptorType == PropertyDescriptorType.Accessor).toMap
  }
}

class ScriptArrayMirror(scriptObject: ObjectReference)(implicit marshaller: Marshaller) extends ScriptObjectMirror(scriptObject) {
  import Mirrors._
  import ScriptObjectMirror._

  def length: Int = invoker.getLength().asNumber(0).intValue()

  def at(index: Int): Value = invoker.applyDynamic(getIntSignature)(index)
}

class ScriptFunctionMirror(scriptObject: ObjectReference)(implicit marshaller: Marshaller) extends ScriptObjectMirror(scriptObject) {
  import Mirrors._

  lazy val name: String = invoker.getName().asString
  lazy val source: String = invoker.toSource().asString
}

object ScriptObjectMirror {
  val putObjectObjectBoolSignature = "put(Ljava/lang/Object;Ljava/lang/Object;Z)Ljava/lang/Object;"
  val getObjectSignature = "get(Ljava/lang/Object;)Ljava/lang/Object;"
  val getIntSignature = "get(I)Ljava/lang/Object;"
}