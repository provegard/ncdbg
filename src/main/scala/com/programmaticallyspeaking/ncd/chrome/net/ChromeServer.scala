package com.programmaticallyspeaking.ncd.chrome.net

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Stash, Status, Terminated}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.programmaticallyspeaking.ncd.chrome.domains.{DomainFactory, DomainMethodArgumentFactory, Messages}
import com.programmaticallyspeaking.ncd.chrome.net.Protocol.Message
import com.programmaticallyspeaking.ncd.infra.ObjectMapping._
import org.slf4s.Logging

import scala.collection.mutable

trait ChromeServer {
  def messageFlow(): Flow[String, Protocol.Message, Any]
}

private sealed trait DevToolsInteraction
private case class FromDevTools(msg: Protocol.IncomingMessage) extends DevToolsInteraction
private case class DevToolsConnected(devToolsRef: ActorRef) extends DevToolsInteraction
private case object DevToolsDisconnected extends DevToolsInteraction

class DevToolsHandler(domainFactory: DomainFactory) extends Actor with Logging with Stash {
  var currentDevToolsRef: Option[ActorRef] = None
  var pendingDevToolsRef: Option[ActorRef] = None
  val domains = mutable.Map[String, ActorRef]()
  val invalidMethods = mutable.Set[String]()

  override def postStop(): Unit = try {
    stopDomainActors()
    domains.clear()
    currentDevToolsRef = None
  } finally super.postStop()

  private def stopDomainActors(): Unit = {
    domains.foreach(e => context.stop(e._2))
  }

  private def disconnectDevTools(status: Any): Unit = {
    currentDevToolsRef.foreach { ref =>
      // If we terminate with Status.Failure we get ugly exception stack traces.
      ref ! Status.Success(status)
    }
    currentDevToolsRef = None
  }

  private def sendToDevTools(message: Protocol.Message): Unit = {
    currentDevToolsRef match {
      case Some(ref) => ref ! message
      case None =>
        log.warn(s"Dropping message $message because there is no Dev Tools client to send it to.")
    }
  }

  private def handleIncomingMessage(msg: Protocol.IncomingMessage) = {
    try {
      val domain = msg.domain()
      // TODO: Test getOrElseUpdate!!
      // TODO: context.watch on the child actor and break if it fails!!
      val domainActor = domains.getOrElseUpdate(domain, {
        log.info(s"Creating a new domain actor for $domain")
        domainFactory.create(domain)
      })
      val domainMessageArg = DomainMethodArgumentFactory.create(msg)

      context.watch(domainActor)

      domainActor ! Messages.Request(msg.id, domainMessageArg)
    } catch {
      case ex: IllegalArgumentException =>
        invalidMethods += msg.method
        log.warn("Failed to handle message: " + ex.getMessage)
        sendToDevTools(Protocol.ErrorResponse(msg.id, "Unknown domain or method: " + msg.method))
      case ex: Exception =>
        // Unknown stuff
        log.error("Failed to handle message", ex)
        sendToDevTools(Protocol.ErrorResponse(msg.id, "ERROR: " + ex.getMessage))
    }
  }

  override def receive: Receive = {
    case DevToolsConnected(actorRef) =>
      currentDevToolsRef = Some(actorRef)
      context.become(receiveClient)
  }

  def waitingForDomainActorsToDie: Receive = {
    case Terminated(domainActorRef) =>
      removeDomainActor(domainActorRef)
      if (domains.isEmpty) {
        pendingDevToolsRef match {
          case Some(ref) =>
            currentDevToolsRef = Some(ref)
            pendingDevToolsRef = None
            context.become(receiveClient)

          case None =>
            // No pending client, so enter listening mode
            context.become(receive)
        }
        unstashAll()
      }

    case msg => stash()
  }

  def receiveClient: Receive = {
    case DevToolsDisconnected =>
      log.info("Developer Tools client disconnected")

      // TODO: Why the heck is this needed? Bug in Akka??
      // https://github.com/jrudolph/akka-http-scala-js-websocket-chat/blob/master/backend/src/main/scala/example/akkawschat/Chat.scala
      // report downstream of completion, otherwise, there's a risk of leaking the
      // downstream when the TCP connection is only half-closed
      disconnectDevTools(Status.Success(Unit))

      if (domains.isEmpty) {
        context.become(receive)
      } else {
        context.become(waitingForDomainActorsToDie)
        // Stop any domain actors - we'll get fresh ones later on
        stopDomainActors()
      }

    case FromDevTools(msg) =>
      log.debug(s"Incoming message from Developer Tools: $msg")

      if (!invalidMethods.contains(msg.method)) {
        handleIncomingMessage(msg)
      }

    case response: Messages.Accepted =>
      log.debug("Got accepted-response (no response data) from domain: " + response)
      sendToDevTools(Protocol.EmptyResponse(response.id))

    case response: Messages.Response =>
      log.debug("Got response from domain: " + response)
      sendToDevTools(Protocol.Response(response.id, response.msg))

    case response: Messages.ErrorResponse =>
      log.warn("Got error response from domain: " + response)
      sendToDevTools(Protocol.ErrorResponse(response.id, response.error))

    case event: Messages.Event =>
      log.debug("Got event from domain: " + event)
      sendToDevTools(Protocol.Event(event.method, event.params))

    case DevToolsConnected(devToolsRef) =>
      log.info(s"New Developer Tools client: $devToolsRef")

      // Disconnect from current Dev Tools
      disconnectDevTools("bye")

      if (domains.isEmpty) {
        // Just set the client reference right away and stay in this context
        currentDevToolsRef = Some(devToolsRef)
      } else {
        // Set the client to pending - it will be made the real one when all domain actors are gone.
        pendingDevToolsRef = Some(devToolsRef)

        context.become(waitingForDomainActorsToDie)

        // Stop any domain actors - we'll get fresh ones later on
        stopDomainActors()
      }

    case Terminated(domainActorRef) =>
      // This happens when a domain actor stops itself to signal shutdown
      removeDomainActor(domainActorRef)
      if (domains.isEmpty) {
        context.become(receive)
        log.info("Observed termination of the last domain actor, disconnecting client.")
        disconnectDevTools("bye")
      }
  }

  private def removeDomainActor(domainActorRef: ActorRef): Unit = {
    val key = domains.find(_._2 == domainActorRef).map(_._1)
    key.foreach(domains.-=)
  }
}

class ChromeServerFactory(domainFactory: DomainFactory)(implicit system: ActorSystem) extends Logging {

  val handler = system.actorOf(Props(new DevToolsHandler(domainFactory)))

  def create(): ChromeServer = {
    def debugInSink = Sink.actorRef[DevToolsInteraction](handler, DevToolsDisconnected)

    new ChromeServer {
      override def messageFlow(): Flow[String, Message, Any] = {
        val in =
          Flow[String]
            .map(toIncomingMessage)
            .to(debugInSink)

        // https://github.com/jrudolph/akka-http-scala-js-websocket-chat/blob/master/backend/src/main/scala/example/akkawschat/Chat.scala
        // The counter-part which is a source that will create a target ActorRef per
        // materialization where the server actor will send its messages to.
        // This source has a limited buffer and will fail if the client doesn't read messages fast enough.
        val out =
          Source.actorRef[Protocol.Message](100, OverflowStrategy.fail)
            .mapMaterializedValue(handler ! DevToolsConnected(_))

        Flow.fromSinkAndSource(in, out)
      }
    }
  }

  private def toIncomingMessage(data: String): FromDevTools = FromDevTools(fromJson[Protocol.IncomingMessage](data))
}
