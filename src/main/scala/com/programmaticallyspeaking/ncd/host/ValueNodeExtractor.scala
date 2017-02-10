package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, Undefined}

import scala.util.{Failure, Success, Try}

trait ObjectInteraction {
  def getOwnProperties(objectId: ObjectId): Map[String, ObjectPropertyDescriptor]

  def invokePropertyGetter(objectId: ObjectId, getter: FunctionNode): Try[ValueNode]
}

class ScriptHostBasedObjectInteraction(scriptHost: ScriptHost) extends ObjectInteraction {
  override def getOwnProperties(objectId: ObjectId): Map[String, ObjectPropertyDescriptor] =
    scriptHost.getObjectProperties(objectId, onlyOwn = true, onlyAccessors = false)

  override def invokePropertyGetter(objectId: ObjectId, getter: FunctionNode): Try[ValueNode] = {
    scriptHost.evaluateOnStackFrame("$top", "(__getter.call(__object))", Map(
      "__getter" -> getter.objectId,
      "__object" -> objectId
    ))
  }
}

class ValueNodeExtractor(objectInteraction: ObjectInteraction) {
  def extract(v: ValueNode): Any = extract(v, Set.empty)

  private def extract(v: ValueNode, observedObjectIds: Set[ObjectId]): Any = v match {
    case SimpleValue(value) if value == Undefined => null
    case SimpleValue(value) => value
    case lzy: LazyNode => extract(lzy.resolve(), observedObjectIds)
    case ArrayNode(_, oid) if observedObjectIds.contains(oid) => s"<Error: cycle detected for array '${oid.id}'>"
    case ArrayNode(items, oid) => Array(items.map(item => extract(item, observedObjectIds + oid)): _*)
    case ObjectNode(oid) if observedObjectIds.contains(oid) => s"<Error: cycle detected for object '${oid.id}'>"
    case ObjectNode(oid) =>
      val props = objectInteraction.getOwnProperties(oid)
      props.map { e =>
        val propName = e._1
        e._2.value match {
          case Some(vn) =>
            propName -> extract(vn, observedObjectIds + oid)
          case None =>
            propName -> (e._2.getter match {
              case Some(g: FunctionNode) =>
                objectInteraction.invokePropertyGetter(oid, g) match {
                  case Success(vn) => extract(vn, observedObjectIds + oid)
                  case Failure(t) => s"<Error calling getter for property '$propName' of object '${oid.id}': ${t.getMessage}>" // TODO!! Handle better!?
                }
              case _ =>
                s"<Error: Unrecognized property '$propName' of object '${oid.id}'>" //TODO: What here?
            })
        }
      }
//      data.map(e => e._1 -> extract(e._2, observedObjectIds + oid))
    case EmptyNode => null
    case DateNode(stringRep, _) => stringRep
    case FunctionNode(name, _, _) => s"<function $name() {}>"
    case ErrorValue(data, _, _) => s"<${data.name}: ${data.message}>"
    // Don't know why I don't get a pattern match warning even though ValueNode is sealed. Is it because
    // LazyNode isn't sealed?
  }
}
