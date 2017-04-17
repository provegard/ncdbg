package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.host.{SimpleValue, ValueNode}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ReflectionFieldMirror
import com.sun.jdi.{ArrayReference, ObjectReference}

trait PropertyHolder {
  def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor]
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