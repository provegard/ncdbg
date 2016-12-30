package com.programmaticallyspeaking.ncd.chrome.domains

object Messages {
  sealed trait DomainMessage
  case class Request(id: String, msg: AnyRef) extends DomainMessage
  case class Accepted(id: String) extends DomainMessage
  case class Response(id: String, msg: Any) extends DomainMessage
  case class ErrorResponse(id: String, error: String) extends DomainMessage
  case class Event(method: String, params: Any) extends DomainMessage
}
