package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject

trait RemoteObjectBuilder {
  def forFunction(name: String, source: String, objectId: String) = {
    val desc = Option(source).getOrElse(s"function $name() { [unknown] }")
    RemoteObject("function", None, Some("Function"), Some(desc), None, None, validObjectId(objectId))
  }

  private def validObjectId(objectId: String): Option[String] = {
    require(objectId != null && objectId.nonEmpty, "object ID is mandatory")
    Some(objectId)
  }

  def forObject(className: String, objectId: String) = {
    RemoteObject("object", None, Some(className), Some(className), None, None, validObjectId(objectId))
  }

  def forObject(value: Map[String, Any]) = {
    // Note: I don't know if object ID should be omitted here. The protocol doesn't say.
    RemoteObject("object", None, Some("Object"), Some("Object"), Some(value), None, None)
  }

  def forArray(length: Int, typedClassName: Option[String], objectId: String) = {
    require(length >= 0, "array length must be non-negative")
    val className = typedClassName.getOrElse("Array")
    val subtype = typedClassName match {
      case Some(_) => "typedarray"
      case None => "array"
    }

    val desc = s"$className[$length]"
    RemoteObject("object", Some(subtype), Some(className), Some(desc), None, None, validObjectId(objectId))
  }

  def forArray(items: Seq[_]) = {
    val desc = s"Array[${items.length}]"
    // Note: I don't know if object ID should be omitted here. The protocol doesn't say.
    RemoteObject("object", Some("array"), Some("Array"), Some(desc), Some(items), None, None)
  }

  def forString(s: String) =
    Option(s).map(str => RemoteObject("string", None, None, None, Some(str), None, None)).getOrElse(nullValue)


  def forNumber(number: Double): RemoteObject = {
    if (number.isNaN || number.isInfinity) {
      return RemoteObject("number", None, None, Some(number.toString), None, Some(number.toString), None)
    }
    if (number == 0 && (1 / number) < 0) {
      // Negative 0
      return RemoteObject("number", None, None, Some("-0"), None, Some("-0"), None)
    }

    RemoteObject("number", None, None, Some(number.toString), Some(number), None, None)
  }

  def forNumber(number: Int): RemoteObject = RemoteObject("number", None, None, Some(number.toString), Some(number), None, None)
  def forNumber(number: Long): RemoteObject = RemoteObject("number", None, None, Some(number.toString), Some(number), None, None)

  val trueValue = RemoteObject("boolean", None, None, None, Some(true), None, None)
  val falseValue = RemoteObject("boolean", None, None, None, Some(false), None, None)
  val nullValue = RemoteObject("object", Some("null"), None, None, Some(null), None, None)
  val undefinedValue = RemoteObject("undefined", None, None, None, None, None, None)

  def forError(name: String, message: String, stack: Option[String], objectId: String) =
    RemoteObject("object", Some("error"), Some(name), Some(stack.getOrElse(s"$name: $message")), None, None, validObjectId(objectId))

  def forDate(stringRepresentation: String, objectId: String) =
    RemoteObject("object", Some("date"), Some("Date"), Some(stringRepresentation), None, None, validObjectId(objectId))

  def forRegExp(stringRepresentation: String, objectId: String) =
    RemoteObject("object", Some("regexp"), Some("RegExp"), Some(stringRepresentation), None, None, validObjectId(objectId))

  //        { type: 'object',
  //                subtype: 'regexp',
  //                className: 'RegExp',
  //                description: '/.*/g',
  //                objectId: '{"injectedScriptId":2,"id":7}' }

}
