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
//    * @param javaStackIncludingMessage stack trace in Java format, with message
    */
  case class ExceptionData(name: String, message: String, lineNumberBase1: Int, columnNumber: Int, url: String, stackIncludingMessage: Option[String])
//                           javaStackIncludingMessage: Option[String])


  sealed trait PropertyDescriptorType
  object PropertyDescriptorType {
    object Generic extends PropertyDescriptorType
    object Data extends PropertyDescriptorType
    object Accessor extends PropertyDescriptorType
  }
  case class ObjectPropertyDescriptor(descriptorType: PropertyDescriptorType, isConfigurable: Boolean, isEnumerable: Boolean, isWritable: Boolean,
                                      isOwn: Boolean,
                                      value: Option[ValueNode], getter: Option[ValueNode], setter: Option[ValueNode]) {
    // Validate input
    descriptorType match {
      case PropertyDescriptorType.Generic =>
        require(value.isEmpty, "Generic descriptor must have no value")
        require(getter.isEmpty, "Generic descriptor must have no getter")
        require(setter.isEmpty, "Generic descriptor must have no setter")

      case PropertyDescriptorType.Data =>
        require(value.isDefined, "Data descriptor must have a value")
        require(getter.isEmpty, "Data descriptor must have no getter")
        require(setter.isEmpty, "Data descriptor must have no setter")

      case PropertyDescriptorType.Accessor =>
        require(value.isEmpty, "Accessor descriptor must have no value")
        require(getter.isDefined || setter.isDefined, "Data descriptor must have getter and/or setter")
    }
  }
}
