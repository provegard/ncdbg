package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.{Actor, ActorRef, DeadLetter, Props}
import com.programmaticallyspeaking.ncd.host.{ScriptEvent, ScriptHost}
import com.programmaticallyspeaking.ncd.testing.{FakeScriptHost, UnitTest}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

object TestDomainActor {
  case object fail
  case class echo(msg: String)
  case class echoLater(msg: String)

  case class customEnable()
}

class TestDomainActor(scriptHost: ScriptHost) extends DomainActor(scriptHost) {
  override protected def handle: PartialFunction[AnyRef, Any] = {
    case TestDomainActor.fail =>
      throw new Exception("I failed")
    case TestDomainActor.echo(msg) =>
      msg
    case TestDomainActor.echoLater(msg) =>
      Future {
        Thread.sleep(200)
        msg
      }
  }

  override protected def handleScriptEvent: PartialFunction[ScriptEvent, Unit] = {
    case AScriptEvent(msg) =>
      emitEvent("TestDomainActor.gotEvent", msg)
  }
}

class TestDomainActorCustomEnable(scriptHost: ScriptHost) extends DomainActor(scriptHost) {
  override protected def isEnable = {
    case TestDomainActor.customEnable() =>
  }
}

case class AScriptEvent(msg: String) extends ScriptEvent

class EnableDisableDomainActor extends DomainActor(FakeScriptHost) {
  override protected def handle: PartialFunction[AnyRef, Any] = {
    case Domain.enable => "enabled"
    case Domain.disable => "disabled"
  }
}

class DomainActorTest extends UnitTest with DomainActorTesting {

  private def enableActor(actor: ActorRef) = requestAndReceive(actor, "1", Domain.enable)

  "A DomainActor actor" - {
    "should handle Domain.enable automatically" in {
      val actor = newActorInstance[TestDomainActor]
      val response = requestAndReceiveResponse(actor, "1", Domain.enable)
      response should be (Accepted)
    }

    "should allow a custom response to Domain.enable" in {
      val actor = newActorInstance[EnableDisableDomainActor]
      val response = requestAndReceiveResponse(actor, "1", Domain.enable)
      response should be ("enabled")
    }

    "should reject Domain.enable if already enabled" in {
      val actor = newActorInstance[TestDomainActor]
      enableActor(actor)
      val ex = intercept[ResponseException] {
        requestAndReceiveResponse(actor, "2", Domain.enable)
      }
      ex.getMessage should include ("already enabled")
    }

    "cannot be enabled twice with a custom enable method" in {
      val actor = newActorInstance[TestDomainActorCustomEnable]
      requestAndReceiveResponse(actor, "1", TestDomainActor.customEnable())
      val ex = intercept[ResponseException] {
        requestAndReceiveResponse(actor, "2", TestDomainActor.customEnable())
      }
      ex.getMessage should include ("already enabled")
    }

    "should handle Domain.disable automatically" in {
      val actor = newActorInstance[TestDomainActor]
      enableActor(actor)
      val response = requestAndReceiveResponse(actor, "2", Domain.disable)
      response should be (Accepted)
    }

    "should allow a custom response to Domain.disable" in {
      val actor = newActorInstance[EnableDisableDomainActor]
      enableActor(actor)
      val response = requestAndReceiveResponse(actor, "2", Domain.disable)
      response should be ("disabled")
    }

    "should reject Domain.disable if not enabled" in {
      val actor = newActorInstance[TestDomainActor]
      val ex = intercept[ResponseException](requestAndReceiveResponse(actor, "1", Domain.disable))
      ex.getMessage should include ("not enabled")
    }

    "should disable the domain on receiving Domain.disable" in {
      val actor = newActorInstance[TestDomainActor]
      enableActor(actor)
      requestAndReceive(actor, "2", Domain.disable)
      val ex = intercept[ResponseException](requestAndReceiveResponse(actor, "3", TestDomainActor.echo("test")))
      ex.getMessage should include ("not enabled")
    }

    "should be able to throw an exception when handling a message" in {
      val actor = newActorInstance[TestDomainActor]

      enableActor(actor)

      val ex = intercept[ResponseException](requestAndReceiveResponse(actor, "2", TestDomainActor.fail))
      ex.getMessage should be ("I failed")
    }

    "should be able to return a value when handling a message" in {
      val actor = newActorInstance[TestDomainActor]

      enableActor(actor)

      val resp = requestAndReceiveResponse(actor, "2", TestDomainActor.echo("hello"))
      resp should be ("hello")
    }

    "should be able to return a Future-wrapped value when handling a message" in {
      val actor = newActorInstance[TestDomainActor]

      enableActor(actor)

      val resp = requestAndReceiveResponse(actor, "2", TestDomainActor.echoLater("hello"))
      resp should be ("hello")
    }

    "should handle a ScriptEvent" in {
      val actor = newActorInstance[TestDomainActor]

      val event = receiveScriptEventTriggeredEvent(actor, Seq(
        Messages.Request("1", Domain.enable)
      ), Seq(
        AScriptEvent("hello")
      ))

      event should be (Messages.Event("TestDomainActor.gotEvent", "hello"))
    }

    "should unsubscribe from script events when stopping" in {
      val deadLettersSubscriber = system.actorOf(Props[CollectorActor], name = "dead-letters-subscriber")
      system.eventStream.subscribe(deadLettersSubscriber, classOf[DeadLetter])

      val actor = newActorInstance[TestDomainActor]
      requestAndReceive(actor, "1", Domain.enable)
      terminateAndWait(actor)

      emitScriptEvent(AScriptEvent("hello"))

      sendAndReceive(deadLettersSubscriber, "collect") match {
        case buf: ListBuffer[_] => buf should be ('empty)
        case other => fail("Unexpected: " + other)
      }
    }
  }
}

class CollectorActor extends Actor {
  private val received = ListBuffer[Any]()
  def receive = {
    case "collect" => sender() ! received
    case msg => received += msg
  }

}