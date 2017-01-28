package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ExceptionData
import com.programmaticallyspeaking.ncd.infra.ObjectMapping

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

sealed trait ValueNode

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
  def extraEntries: Map[String, LazyNode] = Map.empty
}
case object EmptyNode extends ValueNode
case class SimpleValue(value: Any) extends ValueNode

/**
  * Represents a JS Error or an exception.
  *
  * @param data exception data
  * @param isBasedOnThrowable a flag that indicates whether the data are based on a captured [[Throwable]] or if they
  *                           just represent a JS `Error` instance which essentially is a value to be handled like
  *                           any other value.
  * @param objectId object ID
  */
// TODO: Naming - all ComplexNode classes are named XXNode, but not this.
case class ErrorValue(data: ExceptionData, isBasedOnThrowable: Boolean, objectId: ObjectId) extends ComplexNode {
  override def extraEntries: Map[String, LazyNode] = data.javaStackIncludingMessage.map(st => "javaStack" -> LazyNode.eager(SimpleValue(st))).toMap
}

case class ArrayNode(items: Seq[LazyNode], objectId: ObjectId) extends ComplexNode

case class ObjectNode(data: Map[String, LazyNode], objectId: ObjectId) extends ComplexNode {
  override def extraEntries: Map[String, LazyNode] = data
}

case class DateNode(stringRepresentation: String, objectId: ObjectId) extends ComplexNode

/**
  * Represents JavaScript RegExp.
  *
  * @param stringRepresentation the string representation of the RegExp
  * @param objectId object ID, for property retrieval
  */
case class RegExpNode(stringRepresentation: String, objectId: ObjectId) extends ComplexNode

case class FunctionNode(name: String, source: String, objectId: ObjectId) extends ComplexNode