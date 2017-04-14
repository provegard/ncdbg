package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.ActorRef
import com.programmaticallyspeaking.ncd.host.ScriptHost
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.testing.{ActorTesting, FakeScriptHost, UnitTest}

class DefaultDomainFactoryTest extends UnitTest with ActorTesting {
  val container = new Container(Seq(Bar("success")))
  def factory = new DefaultDomainFactory(container)

  "DomainFactory" - {
    "create" - {
      "should create an actor" in {
        factory.create("FooTestDomain") shouldBe an[ActorRef]
      }

      "should give the actor an appropriate name" in {
        val actorRef = factory.create("FooTestDomain")
        actorRef.path.toString should include("FooTestDomain")
      }

      "should not accept 'domain.method'" in {
        assertThrows[IllegalArgumentException](factory.create("FooTestDomain.echo"))
      }

      "should use the container for creating an actor" in {
        val actorRef = factory.create("BarTestDomain")
        actorRef.path.toString should include("BarTestDomain")
        //TODO: Test this, but it requires a ScriptHost actor...
//        val resp = sendAndReceive(actorRef, BarTestDomain.tellMe)
//        resp should be ("success")
      }
    }
  }
}

case class Bar(bar: String)

object BarTestDomain {
  case object tellMe
}

class BarTestDomain(bar: Bar) extends DomainActor(FakeScriptHost) {
  override protected def handle: PartialFunction[AnyRef, Any] = {
    case BarTestDomain.tellMe => bar.bar
  }
}