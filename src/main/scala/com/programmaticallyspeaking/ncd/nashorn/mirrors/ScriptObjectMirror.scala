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

  protected def shouldIncludeProperty(propName: String): Boolean = true

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    // Note: ScriptObject.getPropertyDescriptor seems buggy. When it doesn't find the own property descriptor, it calls
    // _getOwnPropertyDescriptor_ of the proto object, but that means that it won't ever look two levels up. Therefore
    // we have to walk the prototype chain manually. On the plus side, we don't have to use an iterator to get property
    // names and it's easier to separate own from proto descriptors.
    if (onlyOwn)
      ownProperties(markAsOwn = true, onlyAccessors = onlyAccessors)
    else {
      var current: ScriptObjectMirror = this
      var props = Map.empty[String, ObjectPropertyDescriptor]
      while (current != null) {
        props = props ++ current.ownProperties(current eq this, onlyAccessors)
        // getProto of Global returns Global, so make sure uniqueID differs!
        current = invoker.getProto() match {
          case objRef: ObjectReference if objRef.uniqueID() != current.scriptObject.uniqueID() =>
            new ScriptObjectMirror(objRef)
          case _ => null
        }
      }
      props
    }
  }

  private def ownProperties(markAsOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    // Get own properties, pass true to get non-enumerable ones as well (because they are relevant for debugging)
    val propertyNames = getOwnKeys(true)
    var props = propertyNames.filter(shouldIncludeProperty).map { prop =>
      val descriptorToUse = getOwnPropertyDescriptor(prop)
        .getOrElse(throw new IllegalStateException(s"No property descriptor for ${scriptObject.`type`().name()}.$prop"))

      // Read descriptor-generic information
      val theType = descriptorToUse.getType
      val isConfigurable = descriptorToUse.isConfigurable
      val isEnumerable = descriptorToUse.isEnumerable
      val isWritable = descriptorToUse.isWritable
      val isAccessor = theType == 2
      if (onlyAccessors && !isAccessor)
        null // TODO: Fix ugly null usage
      else
        prop -> (theType match {
          case 0 =>
            // Generic
            ObjectPropertyDescriptor(PropertyDescriptorType.Generic, isConfigurable, isEnumerable, isWritable, markAsOwn,
              None, None, None)
          case 1 =>
            // Data, value is ok to use
            val theValue = descriptorToUse.getValue
            ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable, isEnumerable, isWritable, markAsOwn,
              Option(theValue), None, None)
          case 2 =>
            // Accessor, getter/setter are ok to use
            val getter = descriptorToUse.getGetter
            val setter = descriptorToUse.getSetter
            ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, isConfigurable, isEnumerable, isWritable, markAsOwn,
              None, Option(getter), Option(setter))
          case other => throw new IllegalArgumentException("Unknown property descriptor type: " + other)
        })
    }.filterNot(_ == null).toMap

    if (!props.contains(protoName) && shouldIncludeProperty(protoName)) {
      protoProperty.foreach(p => props = props + (protoName -> p))
    }

    props
  }

  private def protoProperty: Option[ObjectPropertyDescriptor] = {
    Option(invoker.getProto()).map { v =>
      val protoValue = Some(marshaller.marshal(v))
      // I don't know if __proto__ is configurable. It is writable though.
      ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = false,
        isWritable = true, isOwn = true, protoValue, None, None)
    }
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
  val protoName = "__proto__"
}