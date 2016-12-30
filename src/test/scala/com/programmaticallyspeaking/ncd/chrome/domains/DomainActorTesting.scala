package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.{ActorRef, Inbox, TypedActor, TypedProps}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Subject
import com.programmaticallyspeaking.ncd.testing.{ActorTesting, UnitTest}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._

import scala.collection.mutable.ListBuffer

class ResponseException(msg: String) extends RuntimeException(msg)

trait DomainActorTesting extends ActorTesting with MockitoSugar { self: UnitTest =>
  import com.programmaticallyspeaking.ncd.testing.MockingUtils._

  case object Accepted

  private val scripts = ListBuffer[Script]()
  private var scriptEventSubject: Subject[ScriptEvent] = _
  protected var objectRegistry: ObjectRegistry = _
  protected var currentScriptHost: ScriptHost = _

  def requestAndReceive(actorRef: ActorRef, id: String, msg: AnyRef): Any = {
    val request = Messages.Request(id, msg)
    sendAndReceive(actorRef, request)
  }

  def requestAndReceiveMatching[T](actorRef: ActorRef, id: String, msg: AnyRef)(pf: PartialFunction[Any, T]): T = {
    val request = Messages.Request(id, msg)
    sendAndReceiveMatching(actorRef, request)(pf)
  }

  def requestAndReceiveResponse(actorRef: ActorRef, id: String, msg: AnyRef): Any = {
    val request = Messages.Request(id, msg)
    sendAndReceiveMatching(actorRef, request) {
      case r: Messages.Response if r.id == id => r.msg
      case r: Messages.ErrorResponse if r.id == id =>
        throw new ResponseException(r.error)
      case r: Messages.Accepted if r.id == id => Accepted
    }
  }

  def requestAndReceiveEvent(actorRef: ActorRef, id: String, msg: AnyRef): Messages.Event = {
    val request = Messages.Request(id, msg)
    sendAndReceiveMatching(actorRef, request) {
      case e: Messages.Event => e
    }
  }

  def receiveScriptEventTriggeredEvent(actorRef: ActorRef, requests: Seq[Messages.Request], scriptEvents: Seq[ScriptEvent]): Messages.Event = {
    val inbox = Inbox.create(system)

    requests.foreach { req =>
      inbox.send(actorRef, req)
      // Wait for a response but ignore it. For example, if the request is Domain.enable, we want the domain to be
      // enabled before we send script events.
      inbox.receive(receiveTimeout)
    }
    scriptEvents.foreach(emitScriptEvent)

    while (true) {
      inbox.receive(receiveTimeout) match {
        case event: Messages.Event => return event
        case _ => // ignore
      }
    }
    ??? // shouldn't get here
  }

  def createScriptHost(): ScriptHost = {
    scriptEventSubject = Subject.serialized[ScriptEvent]
    objectRegistry = new ObjectRegistry {
      override def objectById(id: ObjectId): Option[ComplexNode] = None
    }

    val mockScriptHost = mock[ScriptHost]
    when(mockScriptHost.objectRegistry).thenReturn(objectRegistry)
    when(mockScriptHost.scripts).thenReturn(scripts) // mutable list
    when(mockScriptHost.events).thenReturn(scriptEventSubject)
    when(mockScriptHost.scriptById(any[String])).thenAnswerWith({
      case List(id: String) => scripts.find(_.id == id)
    })
    mockScriptHost
  }

  def addScript(script: Script): Unit = scripts += script

  def emitScriptEvent(event: ScriptEvent): Unit = scriptEventSubject.onNext(event)

  override def beforeTest(): Unit = {
    currentScriptHost = createScriptHost()
    TypedActor(system).typedActorOf(TypedProps(classOf[ScriptHost], currentScriptHost), "scriptHost")
  }
}
