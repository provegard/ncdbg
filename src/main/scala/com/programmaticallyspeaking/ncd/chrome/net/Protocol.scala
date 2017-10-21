package com.programmaticallyspeaking.ncd.chrome.net

object Protocol {
  sealed trait Message

  sealed trait IdentifiableMessage extends Message{
    val id: Long
  }

  case class IncomingMessage(id: Long, method: String, params: Map[String, Any]) extends IdentifiableMessage {
    def domain() = method.split('.').toList match {
      case d :: m :: Nil => d
      case _ => throw new IllegalArgumentException("Not a well-formed method: " + method)
    }
  }
  case class ErrorResponse(id: Long, error: String) extends IdentifiableMessage
  case class EmptyResponse(id: Long) extends IdentifiableMessage {
    val result: Any = Map.empty
  }
  case class Response(id: Long, result: Any) extends IdentifiableMessage

  case class Event(method: String, params: Any) extends Message
}
