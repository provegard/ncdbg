package com.programmaticallyspeaking.ncd.chrome.domains

import java.util.concurrent.Executors

import akka.actor.{ActorRef, Inbox}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.ExecutorProxy
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.messaging.Subject
import com.programmaticallyspeaking.ncd.testing.{ActorTesting, FakeFilePublisher, UnitTest}
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar

import scala.collection.mutable.ListBuffer
import scala.concurrent.TimeoutException
import scala.concurrent.duration._

class ResponseException(msg: String) extends RuntimeException(msg)

trait DomainActorTesting extends ActorTesting with MockitoSugar { self: UnitTest =>
  import com.programmaticallyspeaking.ncd.testing.MockingUtils._

  case object Accepted

  private val scripts = ListBuffer[Script]()
  private var scriptEventSubject: Subject[ScriptEvent] = _
  protected var currentScriptHost: ScriptHost = _
  protected val eventEmitHook: EventEmitHook = new EventEmitHook

  implicit var container: Container = _

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

  def receiveScriptEventTriggeredEvents(actorRef: ActorRef, requests: Seq[Messages.Request], scriptEvents: Seq[ScriptEvent], expectedCount: Int): Seq[Messages.Event] = {
    val inbox = Inbox.create(system)

    requests.foreach { req =>
      inbox.send(actorRef, req)
      // Wait for a response but ignore it. For example, if the request is Domain.enable, we want the domain to be
      // enabled before we send script events.
      inbox.receive(receiveTimeout)
    }
    scriptEvents.foreach(emitScriptEvent)

    val events = ListBuffer[Messages.Event]()
    val before = System.nanoTime()
    while ((System.nanoTime() - before).nanos < receiveTimeout) {
      try {
        inbox.receive(receiveTimeout / 10) match {
          case event: Messages.Event =>
            events += event
            if (events.size >= expectedCount) return events
          case _ => // ignore
        }
      } catch {
        case ex: TimeoutException => // ignore
      }
    }
    events
  }

  def receiveScriptEventTriggeredEvent(actorRef: ActorRef, requests: Seq[Messages.Request], scriptEvents: Seq[ScriptEvent]): Messages.Event = {
    receiveScriptEventTriggeredEvents(actorRef, requests, scriptEvents, 1).headOption match {
      case Some(e) => e
      case None => throw new TimeoutException("Timed out waiting for events")
    }
  }

  def createScriptHost(): ScriptHost = {
    scriptEventSubject = Subject.serialized[ScriptEvent]
    val mockScriptHost = mock[ScriptHost]
    when(mockScriptHost.scripts).thenReturn(scripts) // mutable list
    when(mockScriptHost.events).thenReturn(scriptEventSubject)
    when(mockScriptHost.findScript(any[ScriptIdentity])).thenAnswerWith({
      case List(id: ScriptIdentity) => scripts.find(id.matchesScript)
    })
    mockScriptHost
  }

  def addScript(script: Script): Unit = scripts += script

  def emitScriptEvent(event: ScriptEvent): Unit = scriptEventSubject.onNext(event)

  override def beforeTest(): Unit = {
    currentScriptHost = createScriptHost()
    val scriptHostForContainer = new ExecutorProxy(Executors.newSingleThreadExecutor()).createFor[ScriptHost](currentScriptHost)
    container = new Container(Seq(FakeFilePublisher, scriptHostForContainer, eventEmitHook))
  }
}
