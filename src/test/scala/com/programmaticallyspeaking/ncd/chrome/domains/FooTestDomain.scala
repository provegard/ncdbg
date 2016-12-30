package com.programmaticallyspeaking.ncd.chrome.domains

object FooTestDomain {
  case object bar

  case class baz(a: String, b: Int)

  case class echo(msg: String)

  case object unhandled // Don't handle this in the test actor
}

class FooTestDomain extends DomainActor {
  override protected def handle: PartialFunction[AnyRef, Any] = {
    case FooTestDomain.bar => // noop

    case FooTestDomain.echo(msg) => msg
  }
}
