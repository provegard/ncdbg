package com.programmaticallyspeaking.ncd.host

package object types {
  case object Undefined

  // TODO: Script ID?
  case class ExceptionData(name: String, message: String, lineNumberBase1: Int, columnNumber: Int, url: String, stackIncludingMessage: Option[String])
}
