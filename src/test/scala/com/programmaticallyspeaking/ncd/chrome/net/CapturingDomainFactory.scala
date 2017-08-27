package com.programmaticallyspeaking.ncd.chrome.net

import akka.actor.{Actor, ActorRef, ActorRefFactory, ActorSystem, Inbox, Props, Terminated}
import com.programmaticallyspeaking.ncd.chrome.domains.{DefaultDomainFactory, DomainFactory}
import com.programmaticallyspeaking.ncd.ioc.Container

import scala.concurrent.duration._

class CapturingDomainFactory(implicit container: Container, system: ActorSystem) extends DomainFactory {
  import CapturingDomainFactory._
  private val defaultFactory = new DefaultDomainFactory(container)

  private var actorMustNotExist = false
  private val watcher = system.actorOf(Props(new ActorWatcher))
  private val inbox = Inbox.create(system)

  private def sendAndWait(msg: AnyRef): Any = {
    inbox.send(watcher, msg)
    inbox.receive(2.seconds)
  }

  def actorByName(name: String): Option[ActorRef] = {
    sendAndWait(FindActor(name)) match {
      case FindActorResponse(maybeRef) => maybeRef
      case other => throw new UnsupportedOperationException("Unexpected FindActor response: " + other)
    }
  }

  def requireNoOldActor(): Unit = {
    actorMustNotExist = true
  }

  override def create(domain: String): ActorRef = {
    actorByName(domain) match {
      case Some(ar) if actorMustNotExist =>
        throw new IllegalStateException("Found an old domain actor: " + ar)
      case _ => // noop
    }

    val actor = defaultFactory.create(domain)
    sendAndWait(DomainActor(actor, domain))
    actor
  }
}

object CapturingDomainFactory {

  case class DomainActor(actor: ActorRef, domain: String)
  case class FindActor(domain: String)
  case class FindActorResponse(actor: Option[ActorRef])

  class ActorWatcher extends Actor {

    private var actors = Map[String, ActorRef]()

    override def receive: Receive = {
      case DomainActor(actorRef, domain) =>
        actors += domain -> actorRef
        context.watch(actorRef)
        sender ! "ok"

      case Terminated(actorRef) =>
        actors.find(_._2 == actorRef).map(_._1).foreach(actors -= _)

      case FindActor(domain) =>
        sender ! FindActorResponse(actors.get(domain))
    }
  }
}