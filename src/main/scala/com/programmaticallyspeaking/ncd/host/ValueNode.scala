package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ExceptionData
import com.programmaticallyspeaking.ncd.infra.ObjectMapping

import scala.reflect.ClassTag

/**
  * Identity of an object (or array, or anything else that carries child entries):

  * @param id the string ID
  */
case class ObjectId(id: String) {
  override def toString: String = ObjectMapping.toJson(this)
}

object ObjectId {
  /**
    * Re-creates an [[ObjectId]] from its string representation
    * @param str the string representation of an [[ObjectId]] instance
    */
  def fromString(str: String): ObjectId = {
    try ObjectMapping.fromJson[ObjectId](str) catch {
      case ex: Exception =>
        throw new IllegalArgumentException(s"Not a valid ObjectId string representation: '$str'")
    }
  }
}

sealed trait ValueNode {

  def asBool(dflt: => Boolean): Boolean =
    as[java.lang.Boolean].map(_.booleanValue()).getOrElse(dflt)

  def asNumber(dflt: => Number): Number =
    as[java.lang.Number].getOrElse(dflt)

  def asString: String = as[String].orNull

  def as[R <: AnyRef : ClassTag]: Option[R] = {
    this match {
      case SimpleValue(value: R) => Some(value)
      case other => None
    }
  }

}

object LazyNode {
  /**
    * Creates a [[LazyNode]] instance that isn't lazy.
    *
    * @param value the value that the node will resolve to
    */
  def eager(value: ValueNode) = new LazyNode {
    override def resolve(): ValueNode = value
  }
}
trait LazyNode extends ValueNode {
  def resolve(): ValueNode
}
sealed trait ComplexNode extends ValueNode {
  val objectId: ObjectId
}
case object EmptyNode extends ValueNode
case class SimpleValue(value: Any) extends ValueNode

/** Represents a JS Error or an exception.
  *
  * @param data exception data
  * @param isThrown a flag that indicates whether the error is thrown or not. A non-thrown error is just a value being
  *                 evaluated.
  * @param objectId object ID
  */
// TODO: Naming - all ComplexNode classes are named XXNode, but not this.
case class ErrorValue(data: ExceptionData, isThrown: Boolean, objectId: ObjectId) extends ComplexNode

/**
  * Represents an array.
  *
  * @param size the size of the array
  * @param typedClassName optional class name, `None` for a regular array, a class name for a typed array (e.g. Int8Array)
  * @param objectId the object ID
  */
case class ArrayNode(size: Int, typedClassName: Option[String], objectId: ObjectId) extends ComplexNode

case class ObjectNode(className: String, objectId: ObjectId) extends ComplexNode

case class DateNode(stringRepresentation: String, objectId: ObjectId) extends ComplexNode

/**
  * Represents an ES6 Symbol.
  * @param description the Symbol description
  */
case class SymbolNode(description: String, objectId: ObjectId) extends ComplexNode

/**
  * Represents ES6 Map and WeakMap.
  * @param size the size of the map (unless it's weak, in which case the size is always -1)
  * @param weak if it's a WeakMap
  */
case class MapNode(size: Int, weak: Boolean, objectId: ObjectId) extends ComplexNode

/**
  * Represents ES6 Set and WeakSet.
  * @param size the size of the set (unless it's weak, in which case the size is always -1)
  * @param weak if it's a WeakSet
  */
case class SetNode(size: Int, weak: Boolean, objectId: ObjectId) extends ComplexNode

/**
  * Represents JavaScript RegExp.
  *
  * @param stringRepresentation the string representation of the RegExp
  * @param objectId object ID, for property retrieval
  */
case class RegExpNode(stringRepresentation: String, objectId: ObjectId) extends ComplexNode

case class FunctionNode(name: String, source: String, objectId: ObjectId) extends ComplexNode
