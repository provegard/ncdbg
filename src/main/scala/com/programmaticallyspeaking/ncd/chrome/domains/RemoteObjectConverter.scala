package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ObjectPreview, PropertyPreview, RemoteObject}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.infra.StringAnyMap

class RemoteObjectConverter {

  private def objectId(value: ComplexNode) = value.objectId.toString

  def toRemoteObject(value: ValueNode, byValue: Boolean): RemoteObject = value match {
    case array: ArrayNode =>
      if (byValue) {
        val extractor = new ValueNodeExtractor
        extractor.extract(array) match {
          case a: Array[_] => RemoteObject.forArray(a)
          case other =>
            throw new IllegalStateException("Unexpected extracted value from ArrayNode: " + other)
        }
      } else {
        RemoteObject.forArray(array.items.size, objectId(array))
      }
    case obj: ObjectNode =>
      if (byValue) {
        val extractor = new ValueNodeExtractor
        extractor.extract(obj) match {
          case StringAnyMap(aMap) => RemoteObject.forObject(aMap)
          case other =>
            throw new IllegalStateException("Unexpected extracted value from ObjectNode: " + other)
        }
      } else {
        RemoteObject.forObject(objectId(obj))
      }
    case date: DateNode => RemoteObject.forDate(date.stringRepresentation, objectId(date))
    case regexp: RegExpNode => RemoteObject.forRegExp(regexp.stringRepresentation, objectId(regexp))
    case EmptyNode => RemoteObject.nullValue
    case fun: FunctionNode => RemoteObject.forFunction(fun.name, fun.source, objectId(fun))
    case err: ErrorValue => RemoteObject.forError(err.data.name, err.data.message, err.data.stackIncludingMessage, objectId(err))
    case SimpleValue(b: Boolean) => if (b) RemoteObject.trueValue else RemoteObject.falseValue
    case SimpleValue(s: String) => RemoteObject.forString(s)
    case SimpleValue(n: Int) => RemoteObject.forNumber(n)
    case SimpleValue(n: Long) => RemoteObject.forNumber(n)
    case SimpleValue(n: Number) => RemoteObject.forNumber(n.doubleValue())
    case SimpleValue(Undefined) => RemoteObject.undefinedValue
    case SimpleValue(x) => throw new IllegalArgumentException("Unknown simple value: " + x)
    case other => throw new IllegalArgumentException("Unhandled value: " + other)
  }
}
