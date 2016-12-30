package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ExceptionData

case class ObjectId(id: String)

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
trait ComplexNode extends ValueNode{
  def entries: Seq[(String, LazyNode)]
  val objectId: ObjectId
}
case object EmptyNode extends ValueNode
case class SimpleValue(value: Any) extends ValueNode
case class ErrorValue(data: ExceptionData, objectId: ObjectId) extends ComplexNode {
  override def entries: Seq[(String, LazyNode)] = Seq(
    "message" -> LazyNode.eager(SimpleValue(data.message)),
    "name" -> LazyNode.eager(SimpleValue(data.name))
  ) ++ data.stackIncludingMessage.map(st => "stack" -> LazyNode.eager(SimpleValue(st)))
}
case class ArrayNode(items: Seq[LazyNode], objectId: ObjectId) extends ComplexNode {
  def entries = items.zipWithIndex.map(e => e._2.toString -> e._1)
}
case class ObjectNode(data: Map[String, LazyNode], objectId: ObjectId) extends ComplexNode {
  def entries = data.toSeq
}

case class DateNode(stringRepresentation: String, objectId: ObjectId) extends ComplexNode {
  def entries = Seq.empty
}

case class FunctionNode(name: String, source: String, data: Map[String, LazyNode], objectId: ObjectId) extends ComplexNode {
  def entries = data.toSeq
}