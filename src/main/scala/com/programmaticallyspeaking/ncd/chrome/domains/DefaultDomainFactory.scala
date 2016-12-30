package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.{ActorRef, ActorRefFactory, Props}
import com.programmaticallyspeaking.ncd.infra.IdGenerator

trait DomainFactory {
  def create(domain: String): ActorRef
}

class DefaultDomainFactory(implicit factory: ActorRefFactory) extends DomainFactory {
  private val actorNameIdGenerator = new IdGenerator("domact")

  def create(domain: String): ActorRef = {
    val clazz = lookupActorClass(domain)
    factory.actorOf(Props(clazz), domain + "-" + actorNameIdGenerator.next)
  }

  private def lookupActorClass(domain: String): Class[_] = {
    val className = getClass.getPackage.getName + "." + domain
    val rejection = new IllegalArgumentException("Not a domain actor: " + className)
    try {
      val clazz = Class.forName(className)
      val baseClass = classOf[DomainActor]
      if (baseClass == clazz || !baseClass.isAssignableFrom(clazz)) throw rejection
      clazz
    } catch {
      case ex: ClassNotFoundException => throw rejection
    }
  }
}
