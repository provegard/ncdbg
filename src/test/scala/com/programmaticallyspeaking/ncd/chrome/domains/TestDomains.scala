package com.programmaticallyspeaking.ncd.chrome.domains

import java.util.concurrent.Semaphore

import com.programmaticallyspeaking.ncd.host.ScriptHost
import com.programmaticallyspeaking.ncd.testing.FakeScriptHost

object FooTestDomain {
  case object bar

  case class barArgs(x: Option[String] = None)

  case class baz(a: String, b: Int)

  case class barcase(s: Option[String])

  case class echo(msg: String)

  case object unhandled // Don't handle this in the test actor

  case object takeLock
  case object takeLockEmpty
  case object takeLockError

  private val sem = new Semaphore(0)

  def releaseLock(): Unit = {
    sem.release()
  }
}

class FooTestDomain(host: ScriptHost) extends DomainActor(host, new EventEmitHook) {
  override protected def handle: PartialFunction[AnyRef, Any] = {
    case FooTestDomain.bar => // noop

    case FooTestDomain.echo(msg) => msg

    case FooTestDomain.takeLock =>
      FooTestDomain.sem.acquire()
      "lock done"

    case FooTestDomain.takeLockEmpty =>
      FooTestDomain.sem.acquire()
      ()

    case FooTestDomain.takeLockError =>
      FooTestDomain.sem.acquire()
      throw new RuntimeException("error")
  }
}

object BazTestDomain {
  case class echo(msg: String)
}

class BazTestDomain extends DomainActor(FakeScriptHost, new EventEmitHook) {
  override protected def handle: PartialFunction[AnyRef, Any] = {
    case BazTestDomain.echo(msg) => msg
  }
}
