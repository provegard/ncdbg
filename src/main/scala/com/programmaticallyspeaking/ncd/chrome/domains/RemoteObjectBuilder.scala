package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject

trait RemoteObjectBuilder {
  def forFunction(name: String, source: String, objectId: String) = {
    val desc = Option(source).getOrElse(s"function $name() { [unknown] }")
    RemoteObject("function", null, "Function", desc, null, null, validObjectId(objectId))
  }

  private def validObjectId(objectId: String): String = {
    require(objectId != null && objectId.nonEmpty, "object ID is mandatory")
    objectId
  }

  def forObject(objectId: String) = {
    RemoteObject("object", null, "Object", "Object", null, null, validObjectId(objectId))
  }

  def forObject(value: Map[String, Any]) = {
    // Note: I don't know if object ID should be omitted here. The protocol doesn't say.
    RemoteObject("object", null, "Object", "Object", value, null, null)
  }

  def forArray(length: Int, objectId: String) = {
    require(length >= 0, "array length must be non-negative")
    val desc = s"Array[$length]"
    RemoteObject("object", "array", "Array", desc, null, null, validObjectId(objectId))
  }

  def forArray(items: Seq[_]) = {
    val desc = s"Array[${items.length}]"
    // Note: I don't know if object ID should be omitted here. The protocol doesn't say.
    RemoteObject("object", "array", "Array", desc, items, null, null)
  }

  def forString(s: String) =
    Option(s).map(RemoteObject("string", null, null, null, _, null, null)).getOrElse(nullValue)


  def forNumber(number: Double): RemoteObject = {
    if (number.isNaN || number.isInfinity) {
      return RemoteObject("number", null, null, number.toString, null, number.toString, null)
    }
    if (number == 0 && (1 / number) < 0) {
      // Negative 0
      return RemoteObject("number", null, null, "-0", null, "-0", null)
    }

    RemoteObject("number", null, null, number.toString, number, null, null)
  }

  def forNumber(number: Int): RemoteObject = RemoteObject("number", null, null, number.toString, number, null, null)
  def forNumber(number: Long): RemoteObject = RemoteObject("number", null, null, number.toString, number, null, null)

  val trueValue = RemoteObject("boolean", null, null, null, true, null, null)
  val falseValue = RemoteObject("boolean", null, null, null, false, null, null)
  val nullValue = RemoteObject("object", "null", null, null, null, null, null)
  val undefinedValue = RemoteObject("undefined", null, null, null, null, null, null)

  def forError(name: String, message: String, stack: Option[String], objectId: String) =
    RemoteObject("object", "error", name, stack.getOrElse(s"$name: $message"), null, null, validObjectId(objectId))

  def forDate(stringRepresentation: String, objectId: String) =
    RemoteObject("object", "date", "Date", stringRepresentation, null, null, validObjectId(objectId))

  def forRegExp(stringRepresentation: String, objectId: String) =
    RemoteObject("object", "regexp", "RegExp", stringRepresentation, null, null, validObjectId(objectId))

  //        { type: 'object',
  //                subtype: 'regexp',
  //                className: 'RegExp',
  //                description: '/.*/g',
  //                objectId: '{"injectedScriptId":2,"id":7}' }

}
