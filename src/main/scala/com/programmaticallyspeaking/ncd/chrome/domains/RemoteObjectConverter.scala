package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.infra.ObjectMapping

class RemoteObjectConverter() {

  private def objectId(value: ComplexNode) = ObjectMapping.toJson(value.objectId)

  def toRemoteObject(value: ValueNode): RemoteObject = value match {
    case array: ArrayNode => RemoteObject.forArray(array.items.size, objectId(array))
    case obj: ObjectNode => RemoteObject.forObject(objectId(obj))
    case date: DateNode => RemoteObject.forDate(date.stringRepresentation, objectId(date))
    case EmptyNode => RemoteObject.nullValue
    case fun: FunctionNode => RemoteObject.forFunction(fun.name, fun.source, objectId(fun))
    case err: ErrorValue => RemoteObject.forError(err.data.name, err.data.message, err.data.stackIncludingMessage, objectId(err))
    case SimpleValue(b: Boolean) => if (b) RemoteObject.trueValue else RemoteObject.falseValue
    case SimpleValue(s: String) => RemoteObject.forString(s)
    case SimpleValue(n: Number) => RemoteObject.forNumber(n.doubleValue())
    case SimpleValue(Undefined) => RemoteObject.undefinedValue
    case SimpleValue(x) => throw new IllegalArgumentException("Unknown simple value: " + x)
    case other => throw new IllegalArgumentException("Unhandled value: " + other)
  }
}
