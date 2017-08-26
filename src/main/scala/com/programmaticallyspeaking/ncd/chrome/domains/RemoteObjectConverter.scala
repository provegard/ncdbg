package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.infra.StringAnyMap

object RemoteObjectConverter {
  def byValue(objectInteraction: ObjectInteraction) = new ByValueRemoteObjectConverter(objectInteraction)
  def byReference = new ByReferenceRemoteObjectConverter
}

trait RemoteObjectConverter {
  def toRemoteObject(value: ValueNode): RemoteObject
}

class ByReferenceRemoteObjectConverter extends RemoteObjectConverter {

  private def objectId(value: ComplexNode) = value.objectId.toString

  def toRemoteObject(value: ValueNode): RemoteObject = value match {
    case array: ArrayNode => RemoteObject.forArray(array.size, array.typedClassName, objectId(array))
    case sl: ScopeList => RemoteObject.forScopes(sl.size, objectId(sl))
    case so: ScopeObject => RemoteObject.forScope(so.scopeType, so.name, objectId(so))
    case obj: ObjectNode => RemoteObject.forObject(obj.className, objectId(obj))
    case date: DateNode => RemoteObject.forDate(date.stringRepresentation, objectId(date))
    case regexp: RegExpNode => RemoteObject.forRegExp(regexp.stringRepresentation, objectId(regexp))
    case sym: SymbolNode => RemoteObject.forSymbol(sym.description, objectId(sym))
    case map: MapNode => RemoteObject.forMap(map.size, map.weak, objectId(map))
    case set: SetNode => RemoteObject.forSet(set.size, set.weak, objectId(set))
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

class ByValueRemoteObjectConverter(objectInteraction: ObjectInteraction) extends ByReferenceRemoteObjectConverter {
  private lazy val extractor = new ValueNodeExtractor(objectInteraction)

  override def toRemoteObject(value: ValueNode): RemoteObject = value match {
    case array: ArrayNode =>
      extractor.extract(array) match {
        case a: Array[_] => RemoteObject.forArray(a)
        case other =>
          throw new IllegalStateException("Unexpected extracted value from ArrayNode: " + other)
      }
    case obj: ObjectNode =>
      extractor.extract(obj) match {
        case StringAnyMap(aMap) => RemoteObject.forObject(aMap)
        case other =>
          throw new IllegalStateException("Unexpected extracted value from ObjectNode: " + other)
      }
    case other => super.toRemoteObject(value)
  }
}