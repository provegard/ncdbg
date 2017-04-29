package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.sun.jdi.{ArrayReference, Method, ObjectReference}

import scala.collection.mutable

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

  import scala.collection.JavaConverters._
  private val refType = obj.referenceType()

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    var props = javaBeansMap(onlyOwn)
    if (!onlyAccessors) props = fieldMap(onlyOwn) ++ props
    props
  }

  private def fieldMap(onlyOwn: Boolean): Map[String, ObjectPropertyDescriptor] = {
    val fields = if (onlyOwn) refType.fields() else refType.allFields()
    fields.asScala.map { f =>
      val isOwn = f.declaringType() == refType
      val theValue = marshaller.marshal(obj.getValue(f))
      f.name() -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
        isWritable = !f.isFinal, isOwn = isOwn, Some(theValue), None, None)
    }.toMap
  }

  private def javaBeansMap(onlyOwn: Boolean): Map[String, ObjectPropertyDescriptor] = {
    val methods = if (onlyOwn) refType.methods() else refType.allMethods() //TODO: Test non-all here!
    methods.asScala
      .flatMap(methodToJavaBeanMethod) // filter out JavaBean methods
      .groupBy(_.propertyName)
      .filter(g => fitTogether(g._2))
      .map { g =>
        val getter = g._2.find(_.isGetter).map(_.toFunctionNode)
        val setter = g._2.find(!_.isGetter).map(_.toFunctionNode)
        val isOwn = g._2.head.method.declaringType() == refType
        g._1 -> ObjectPropertyDescriptor(PropertyDescriptorType.Accessor,
          false, true, setter.isDefined, isOwn, None, getter, setter)
      }
  }
}

object ArbitraryObjectPropertyHolder {
  import scala.collection.JavaConverters._

  private case class JavaBeansMethod(propertyName: String, method: Method, isGetter: Boolean) {
    private[nashorn] def toFunctionNode: FunctionNode = {
      // As far as I can tell, DevTools doesn't care about a property getter/setter other than what they signify.
      // When a getter is clicked in the UI, the property value is fetched "normally". This is good, because I don't
      // know how to marshal a JDI Method as an ObjectReference via Marshaller.
      val id = Seq(method.declaringType().name(), method.name(), method.signature()).mkString(";")
      val oid = ObjectId(id)
      FunctionNode(method.name(), s"function ${method.name()}() { [native code] }", oid)
    }
  }

  private val nameRegexp = "^[gs]et(\\p{Lu})(.*)".r
  private val VoidTypeName = "void"

  private[nashorn] def extractJavaBeansPropertyName(name: String): Option[String] = name match {
    case nameRegexp(first, rest) => Some(first.toLowerCase + rest)
    case _ => None
  }

  private def methodToJavaBeanMethod(method: Method): Option[JavaBeansMethod] = {
    extractJavaBeansPropertyName(method.name()).map { propName =>
      JavaBeansMethod(propName, method, method.name().startsWith("get"))
    }.filter(meetsBeanRequirements)
  }

  private def meetsBeanRequirements(m: JavaBeansMethod): Boolean = {
    if (m.isGetter) {
      m.method.returnTypeName() != VoidTypeName && m.method.argumentTypeNames().isEmpty
    } else {
      m.method.returnTypeName() == VoidTypeName && m.method.argumentTypeNames().size() == 1
    }
  }

  // Assumes meetsBeanRequirements is true for both
  private def fitTogether(ms: Seq[JavaBeansMethod]): Boolean = {
    // Getter/setter or only one
    ms.size <= 2 &&
    // isGetter must be different across all
      ms.map(_.isGetter).distinct.size == ms.size &&
    // all must belong to the same class
      ms.map(_.method.declaringType()).distinct.size == 1 &&
    // All non-void types should be the same (essentially this is a return-parameter type match)
      ms.flatMap(m => m.method.argumentTypeNames().asScala :+ m.method.returnTypeName()).filter(_ != VoidTypeName).distinct.size == 1
  }
}

class HashtablePropertyHolder(table: ObjectReference)(implicit marshaller: Marshaller) extends PropertyHolder {
  import com.programmaticallyspeaking.ncd.nashorn.mirrors.Mirrors._
  private val tableInvoker = new DynamicInvoker(marshaller.thread, table)

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
    if (onlyAccessors) return Map.empty // Hashtable cannot have accessor properties

    val enumeration = tableInvoker.keys()
    val enumInvoker = new DynamicInvoker(marshaller.thread, enumeration.asInstanceOf[ObjectReference])
    val result = mutable.Map[String, ObjectPropertyDescriptor]()
    while (enumInvoker.hasMoreElements().asBool(false)) {
      val keyValue = enumInvoker.next()
      val marshalledKey = marshaller.marshal(keyValue)

      // Keys in a JS object are strings
      val keyAsString = marshalledKey match {
        case SimpleValue(something) => something.toString
        case _ if keyValue.isInstanceOf[ObjectReference] =>
          val keyInvoker = new DynamicInvoker(marshaller.thread, keyValue.asInstanceOf[ObjectReference])
          keyInvoker.applyDynamic("toString")().asString
        case _ => throw new RuntimeException("Unknown Hashtable key: " + keyValue)
      }

      val value: ValueNode = tableInvoker.get(keyValue)

      result += keyAsString -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, true, true, true, Some(value), None, None)
    }
    result.toMap
  }
}