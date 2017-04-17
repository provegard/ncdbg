package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.host.{SimpleValue, ValueNode}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.{JSObjectMirror, ReflectionFieldMirror, ScriptObjectMirror}
import com.sun.jdi.{ArrayReference, ObjectReference}

trait PropertyHolder {
  def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor]
}

class JSObjectPropertyHolder(jsObject: ObjectReference, isArray: Boolean)(implicit marshaller: Marshaller) extends PropertyHolder {
  import com.programmaticallyspeaking.ncd.infra.StringUtils._
  private val mirror = new JSObjectMirror(jsObject)

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {

    // For an array, keySet should return indices + "length", and then we get use getSlot.
    val properties = mirror.keySet()

    properties.map { prop =>
      val theValue =
        if (isArray && isUnsignedInt(prop)) mirror.getSlot(prop.toInt) else mirror.getMember(prop)

      // Note: A ValueNode shouldn't be null/undefined, so use Some(...) rather than Option(...) for the value
      prop -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
        isWritable = true, isOwn = true, Some(theValue), None, None)
    }.toMap
  }
}

class ArrayPropertyHolder(array: ArrayReference)(implicit marshaller: Marshaller) extends PropertyHolder {
  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    // Note: A ValueNode shouldn't be null/undefined, so use Some(...) rather than Option(...) for the value
    def createProp(value: ValueNode) =
      ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
        isWritable = true, isOwn = true, Some(value), None, None)

    // Just return index properties + length.
    val props = (0 until array.length()).map { idx =>
      val theValue = marshaller.marshal(array.getValue(idx))
      idx.toString -> createProp(theValue)
    } :+ ("length" -> createProp(SimpleValue(array.length())))
    props.toMap
  }
}

class ArbitraryObjectPropertyHolder(obj: ObjectReference)(implicit marshaller: Marshaller) extends PropertyHolder {
  import scala.collection.JavaConverters._
  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    val invoker = new DynamicInvoker(marshaller.thread, obj)
    val clazz = invoker.applyDynamic("getClass")().asInstanceOf[ObjectReference]
    val classInvoker = new DynamicInvoker(marshaller.thread, clazz)

    // TODO: Handle onlyOwn == false
    classInvoker.getDeclaredFields() match {
      case arr: ArrayReference =>
        arr.getValues.asScala.map { f =>
          val mirror = new ReflectionFieldMirror(f.asInstanceOf[ObjectReference])
          val isAccessible = mirror.isAccessible
          if (!isAccessible) {
            mirror.setAccessible(true)
          }
          try {
            val theValue = mirror.get(obj)

            // TODO: isOwn depends on input (see TODO above)
            mirror.name -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
              isWritable = !mirror.isFinal, isOwn = true, Some(theValue), None, None)
          } finally {
            if (!isAccessible) {
              mirror.setAccessible(false)
            }
          }

        }.toMap
      case _ => Map.empty
    }
  }
}

class ScriptObjectPropertyHolder(scriptObject: ObjectReference)(implicit marshaller: Marshaller) extends PropertyHolder {
  private val mirror = new ScriptObjectMirror(scriptObject)

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    val propertyNames = if (onlyOwn) {
      // Get own properties, pass true to get non-enumerable ones as well (because they are relevant for debugging)
      mirror.getOwnKeys(true)
    } else {
      // Get all properties - this method walks the prototype chain
      mirror.propertyIterator().toArray
    }
    propertyNames.map { prop =>
      // Get either only the own descriptor or try both ways (own + proto). This is required for us to know
      // if the descriptor represents an own property.
      val ownDescriptor = mirror.getOwnPropertyDescriptor(prop)
      // TODO ugly, ugly.. make nicer
      val hasOwnDescriptor = ownDescriptor.isDefined

      val protoDescriptor = if (onlyOwn || hasOwnDescriptor) None else mirror.getPropertyDescriptor(prop)

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