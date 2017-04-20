package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.host.{SimpleValue, ValueNode}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.{ClassMirror, ReflectionFieldMirror, ReflectionMethodMirror}
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
  import ArbitraryObjectPropertyHolder._

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    val invoker = new DynamicInvoker(marshaller.thread, obj)
    val clazz = invoker.applyDynamic("getClass")().asInstanceOf[ObjectReference]

    val fields = fieldsFor(clazz, own = true) ++ javaBeansFor(clazz)
    if (onlyOwn) fields else {
      // Visit parent classes as well
      def parents(c: ObjectReference): Seq[ObjectReference] = {
        val invoker = new DynamicInvoker(marshaller.thread, c)
        invoker.getSuperclass() match {
          case sc: ObjectReference => Seq(sc) ++ parents(sc)
          case _ => Seq.empty
        }
      }
      val pp = parents(clazz)
      fields ++ pp.flatMap(c => fieldsFor(c, own = false))
    }
  }

  private def javaBeansFor(clazz: ObjectReference): Map[String, ObjectPropertyDescriptor] = {
    val cmirror = new ClassMirror(clazz)
    cmirror.publicMethods
      .flatMap(methodToJavaBeanMethod) // filter out JavaBean methods
      .groupBy(_.propertyName)
      .filter(g => fitTogether(g._2))
      .map { g =>
        val getter = g._2.find(_.isGetter).map(_.mirror.asFunctionNode)
        val setter = g._2.find(!_.isGetter).map(_.mirror.asFunctionNode)
        val isOwn = g._2.head.mirror.declaringClassName == cmirror.name
        g._1 -> ObjectPropertyDescriptor(PropertyDescriptorType.Accessor,
          false, true, setter.isDefined, isOwn, None, getter, setter)
      }
      .toMap
  }

  private def fieldsFor(clazz: ObjectReference, own: Boolean): Map[String, ObjectPropertyDescriptor] = {
    val cmirror = new ClassMirror(clazz)
    cmirror.declaredFields.map { mirror =>
      val isAccessible = mirror.isAccessible
      if (!isAccessible) {
        mirror.setAccessible(true)
      }
      try {
        val theValue = mirror.get(obj)

        mirror.name -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
          isWritable = !mirror.isFinal, isOwn = own, Some(theValue), None, None)
      } finally {
        if (!isAccessible) {
          mirror.setAccessible(false)
        }
      }

    }.toMap
  }

}

object ArbitraryObjectPropertyHolder {
  private case class JavaBeansMethod(propertyName: String, mirror: ReflectionMethodMirror, isGetter: Boolean)

  private val nameRegexp = "^[gs]et(\\p{Lu})(.*)".r
  private val VoidTypeName = "void"

  private[nashorn] def extractJavaBeansPropertyName(name: String): Option[String] = name match {
    case nameRegexp(first, rest) => Some(first.toLowerCase + rest)
    case _ => None
  }

  private def methodToJavaBeanMethod(mirror: ReflectionMethodMirror): Option[JavaBeansMethod] = {
    extractJavaBeansPropertyName(mirror.name).map { propName =>
      JavaBeansMethod(propName, mirror, mirror.name.startsWith("get"))
    }.filter(meetsBeanRequirements)
  }

  private def meetsBeanRequirements(m: JavaBeansMethod): Boolean = {
    if (m.isGetter) {
      m.mirror.returnTypeName != VoidTypeName && m.mirror.parameterTypeNames.isEmpty
    } else {
      m.mirror.returnTypeName == VoidTypeName && m.mirror.parameterTypeNames.size == 1
    }
  }

  // Assumes meetsBeanRequirements is true for both
  private def fitTogether(ms: Seq[JavaBeansMethod]): Boolean = {
    // Getter/setter or only one
    ms.size <= 2 &&
    // isGetter must be different across all
      ms.map(_.isGetter).distinct.size == ms.size &&
    // all must belong to the same class
      ms.map(_.mirror.declaringClassName).distinct.size == 1 &&
    // All non-void types should be the same (essentially this is a return-parameter type match)
      ms.flatMap(m => m.mirror.parameterTypeNames :+ m.mirror.returnTypeName).filter(_ != VoidTypeName).distinct.size == 1
  }
}