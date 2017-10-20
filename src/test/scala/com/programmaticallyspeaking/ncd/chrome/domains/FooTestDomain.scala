package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.testing.FakeScriptHost

object FooTestDomain {
  case object bar

  case class baz(a: String, b: Int)

  case class barcase(s: Option[String])

  case class echo(msg: String)

  case object unhandled // Don't handle this in the test actor
}

class FooTestDomain extends DomainActor(FakeScriptHost) {
  override protected def handle: PartialFunction[AnyRef, Any] = {
    case FooTestDomain.bar => // noop

    case FooTestDomain.echo(msg) => msg
  }
}
