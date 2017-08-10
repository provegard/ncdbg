package com.programmaticallyspeaking.ncd.testing

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}

import scala.util.{Failure, Try}

class MapBasedObjectInteraction(data: Map[ObjectId, Map[String, ValueNode]]) extends ObjectInteraction {

  override def getOwnProperties(objectId: ObjectId): Seq[(String, ObjectPropertyDescriptor)] = data.get(objectId) match  {
    case Some(objectData) =>
      objectData.map { e =>
        e._1 -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, true, false, true, Some(e._2), None, None)
      }.toSeq
    case None => throw new IllegalArgumentException("Unknown object ID: " + objectId.id)
  }

  override def invokePropertyGetter(objectId: ObjectId, getter: FunctionNode): Try[ValueNode] =
    Failure(new UnsupportedOperationException("MapBasedObjectInteraction doesn't support function invocation"))
}
