package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.Undefined

class ValueNodeExtractor {
  def extract(v: ValueNode): Any = extract(v, Set.empty)

  private def extract(v: ValueNode, observedObjectIds: Set[ObjectId]): Any = v match {
    case SimpleValue(value) if value == Undefined => null
    case SimpleValue(value) => value
    case lzy: LazyNode => extract(lzy.resolve(), observedObjectIds)
    case ArrayNode(_, oid) if observedObjectIds.contains(oid) => s"<Error: cycle detected for array '${oid.id}'>"
    case ArrayNode(items, oid) => Array(items.map(item => extract(item, observedObjectIds + oid)): _*)
    case ObjectNode(_, oid) if observedObjectIds.contains(oid) => s"<Error: cycle detected for object '${oid.id}'>"
    case ObjectNode(data, oid) => data.map(e => e._1 -> extract(e._2, observedObjectIds + oid))
    case EmptyNode => null
    case DateNode(stringRep, _) => stringRep
    case FunctionNode(name, _, _, _) => s"<function $name() {}>"
    case ErrorValue(data, _, _) => s"<${data.name}: ${data.message}>"
    // Don't know why I don't get a pattern match warning even though ValueNode is sealed. Is it because
    // LazyNode isn't sealed?
  }
}
