package com.programmaticallyspeaking.ncd.host

package object types {
  case object Undefined

  /**
    * Contains exception data.
    *
    * @param name the name of the exception/error type (e.g. TypeError for a JS type error)
    * @param message the exception message
    * @param lineNumberBase1 1-based line number where the error occurred
    * @param columnNumber column number (base ??) where the error occurred
    * @param url URL of the script in which the error occurred
    * @param stackIncludingMessage stack trace (JS format), with message (for consistency with Error.prototype.stack)
    * @param javaStackIncludingMessage stack trace in Java format, with message
    */
  case class ExceptionData(name: String, message: String, lineNumberBase1: Int, columnNumber: Int, url: String, stackIncludingMessage: Option[String],
                           javaStackIncludingMessage: Option[String])
}
