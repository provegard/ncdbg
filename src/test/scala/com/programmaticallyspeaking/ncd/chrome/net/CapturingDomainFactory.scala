package com.programmaticallyspeaking.ncd.chrome.net

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorRefFactory, Props, Terminated}
import com.programmaticallyspeaking.ncd.chrome.domains.{DefaultDomainFactory, DomainFactory}

import scala.collection.concurrent.TrieMap

class CapturingDomainFactory(dflt: Option[DomainFactory] = None)(implicit factory: ActorRefFactory) extends DomainFactory {
  private val defaultFactory = dflt.getOrElse(new DefaultDomainFactory())

  private var actorMustNotExist = false
  private val actors = TrieMap[String, ActorRef]()
  private val watcher = factory.actorOf(Props(new ActorWatcher))

  def actorByName(name: String): Option[ActorRef] = actors.get(name)

  def requireNoOldActor(): Unit = {
    actorMustNotExist = true
  }

  override def create(domain: String): ActorRef = {
    actors.get(domain) match {
      case Some(ar) if actorMustNotExist => throw new IllegalStateException("Found an old domain actor: " + ar)
      case _ => // noop
    }
    val actor = defaultFactory.create(domain)
    watcher ! actor
    actors(domain) = actor
    actor
  }

  class ActorWatcher extends Actor {
    override def receive: Receive = {
      case actorRef: ActorRef =>
        context.watch(actorRef)
      case Terminated(actorRef) =>
        val key = actors.find(_._2 == actorRef).map(_._1)
        key.foreach(actors.-=)
    }
  }
}
