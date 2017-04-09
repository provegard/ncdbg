package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.{ActorRef, ActorRefFactory, Props}
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.programmaticallyspeaking.ncd.ioc.Container

trait DomainFactory {
  def create(domain: String): ActorRef
}

class DefaultDomainFactory(container: Container)(implicit factory: ActorRefFactory) extends DomainFactory {
  private val actorNameIdGenerator = new IdGenerator("domact")

  def create(domain: String): ActorRef = {
    val clazz = lookupActorClass(domain)
    def creator(clazz: Class[_], args: Seq[Any]) = Props(clazz, args: _*)
    factory.actorOf(container.newInstance(clazz, creator), domain + "-" + actorNameIdGenerator.next)
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
