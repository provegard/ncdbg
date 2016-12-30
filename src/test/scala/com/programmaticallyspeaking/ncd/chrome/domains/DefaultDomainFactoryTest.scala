package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.ActorRef
import com.programmaticallyspeaking.ncd.testing.{ActorTesting, UnitTest}
import org.scalatest.Outcome

class DefaultDomainFactoryTest extends UnitTest with ActorTesting {

  def factory = new DefaultDomainFactory()

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
    }
  }
}
