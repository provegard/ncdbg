package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.net.Protocol
import com.programmaticallyspeaking.ncd.infra.ObjectMapping

import scala.util.{Failure, Success, Try}

object DomainMethodArgumentFactory {
  def create(message: Protocol.IncomingMessage): AnyRef = {
    message.method.split('.').toList match {
      case domain :: method :: Nil =>
        privCreate(domain, method, message.params)

      case _ =>
        throw new IllegalArgumentException("Malformed method: " + message.method)
    }
  }

  private def isEnableOrDisable(method: String) = method == "enable" || method == "disable"

  private def privCreate(aDomain: String, method: String, params: Map[String, Any]): AnyRef = {
    var domain = aDomain
    val hasParams = params != null

    // Special method handling - enable and disable are generic (unless there are parameters)
    if (isEnableOrDisable(method) && !hasParams) domain = "Domain"

    val className = DomainMethodArgumentFactory.getClass.getPackage.getName + "." + domain + "$" + method
    val maybeClasses = lookupClasses(className)
    val caseObjectClass = maybeClasses.caseObject.getOrElse(throw rejection(domain, method, "the domain and/or method are unknown"))

    if (hasParams) {
      // Create a case class instance
      maybeClasses.caseClass match {
        case Some(caseClass) =>
          ObjectMapping.fromMap(params, caseClass).asInstanceOf[AnyRef]
        case None =>
          // There are arguments, so we ended up here in the case class branch. But the method refers to a case object.
          throw rejection(domain, method, "there are arguments")
      }
    } else {
      // Return the case object instance.
      maybeClasses.caseClass match {
        case Some(_) =>
          // However, there is a case class, so this domain method expects arguments!
          throw rejection(domain, method, "arguments are missing")
        case None =>
          // Creating a new instance of the case class isn't stable, since whether or not we get the correct instance
          // depends on if the case object has been initialized. Instead, we read the value of the MODULE$ field.
          val field = caseObjectClass.getField("MODULE$")
          field.get(null)
      }
    }
  }

  private def rejection(domain: String, method: String, reason: String): Exception =
    new IllegalArgumentException(s"Cannot create domain method $domain.$method because $reason")

  private def lookupClasses(className: String): Classes =
    Classes(lookupClass(className), lookupClass(className + "$"))

  private def lookupClass(name: String): Option[Class[_]] = Try(Class.forName(name)) match {
    case Success(clazz) => Some(clazz)
    case Failure(t) => None
  }

  case class Classes(caseClass: Option[Class[_]], caseObject: Option[Class[_]])
}
