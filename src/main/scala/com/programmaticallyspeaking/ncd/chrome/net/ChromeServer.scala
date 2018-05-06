package com.programmaticallyspeaking.ncd.chrome.net

import java.net.ConnectException
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, OneForOneStrategy, Props, Stash, Status, SupervisorStrategy, Terminated}
import com.programmaticallyspeaking.ncd.chrome.domains.{DomainFactory, DomainMethodArgumentFactory, Messages}
import com.programmaticallyspeaking.ncd.chrome.net.Protocol.Message
import com.programmaticallyspeaking.ncd.messaging.{Observable, Observer, SerializedSubject}
import org.slf4s.Logging

import scala.collection.mutable

trait ChromeServer {
  def connect(): Observable[Protocol.Message]
  def disconnect(): Unit
  def sendMessage(message: Protocol.IncomingMessage): Unit
}

private sealed trait DevToolsInteraction
private case class FromDevTools(msg: Protocol.IncomingMessage) extends DevToolsInteraction
private case object DevToolsConnected extends DevToolsInteraction
private case object DevToolsDisconnected extends DevToolsInteraction

class DevToolsHandler(domainFactory: DomainFactory) extends Actor with Logging with Stash {
  var currentDevToolsRef: Option[ActorRef] = None
  var pendingDevToolsRef: Option[ActorRef] = None
  val domains = mutable.Map[String, ActorRef]()
  val invalidMethods = mutable.Set[String]()

  // Queue for requests that we won't send because there are outstanding requests. We serialize all requests
  // to avoid race conditions between domain actors (e.g. resuming the VM when another domain is getting properties).
  private val queuedRequests = mutable.Queue[(Protocol.IncomingMessage, Messages.Request)]()

  private val outstandingRequests = mutable.Set[String]()

  override val supervisorStrategy: SupervisorStrategy =
    new OneForOneStrategy(SupervisorStrategy.defaultDecider) {
      override def logFailure(context: ActorContext, child: ActorRef, cause: Throwable, decision: SupervisorStrategy.Directive): Unit = cause match {
        case t: Exception if isConnectException(t) =>
          // noop, Broker should have logged this
        case _ =>
          super.logFailure(context, child, cause, decision)
      }
    }

  private def isConnectException(t: Throwable): Boolean = t match {
    case _: ConnectException => true
    case t: RuntimeException if isConnectException(t.getCause) => true
    case _ => false
  }

  override def postStop(): Unit = try {
    stopDomainActors()
    domains.clear()
    currentDevToolsRef = None
  } finally super.postStop()

  private def stopDomainActors(): Unit = {
    domains.foreach(e => context.stop(e._2))
  }

  private def disconnectDevTools(status: Any): Unit = {
    queuedRequests.clear()
    outstandingRequests.clear()

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

  private def getOrCreateDomainActor(domain: String): ActorRef = {
    val domainActor = domains.getOrElseUpdate(domain, {
      val actorRef = domainFactory.create(domain)
      log.trace(s"Created a new domain actor for $domain: $actorRef")
      actorRef
    })

    context.watch(domainActor)

    domainActor
  }

  private def handleIncomingMessage(msg: Protocol.IncomingMessage): Unit = {
    if (invalidMethods.contains(msg.method)) {
      sendToDevTools(Protocol.ErrorResponse(msg.id, "Unknown domain or method: " + msg.method))
      return
    }
    try {
      val domain = msg.domain()
      val domainMessageArg = DomainMethodArgumentFactory.create(msg)
      val request = Messages.Request(msg.id.toString, domainMessageArg)

      // If there are no outstanding requests, make this one (but it's now outstanding).
      // Otherwise queue it.
      if (outstandingRequests.isEmpty) {
        sendRequestToDomainActor(domain, request)
      } else {
        queuedRequests += ((msg, request))
      }
    } catch {
      case ex: IllegalArgumentException =>
        handleInvalidDomainOrMethod(msg, ex)
      case ex: Exception =>
        // Unknown stuff
        log.error(s"Failed to handle message ($msg)", ex)
        sendToDevTools(Protocol.ErrorResponse(msg.id, "ERROR: " + ex.getMessage))
    }
  }

  private def handleInvalidDomainOrMethod(msg: Protocol.IncomingMessage, ex: IllegalArgumentException): Unit = {
    invalidMethods += msg.method
    log.warn(s"Failed to handle message ($msg): " + ex.getMessage)
    sendToDevTools(Protocol.ErrorResponse(msg.id, "Unknown domain or method: " + msg.method))
  }

  private def sendRequestToDomainActor(domain: String, request: Messages.Request): Unit = {
    val actor = getOrCreateDomainActor(domain)
    outstandingRequests += request.id
    actor ! request
  }

  private def sendQueuedRequest(msg: Protocol.IncomingMessage, request: Messages.Request): Unit = {
    try sendRequestToDomainActor(msg.domain(), request) catch {
      case ex: IllegalArgumentException =>
        // DomainMethodArgumentFactory.create in handleIncomingMessage won't catch an unknown domain in case
        // of X.enable, since that actually gets transformed into Domain.enable. Thus, for a queued request we'll
        // get the unknown domain error here instead, when actor creation is attempted.
        handleInvalidDomainOrMethod(msg, ex)

        // We won't get a request response and thus won't hit handleRequestResponse, so explicit
        // queue processing is needed.
        processQueuedRequests()
    }
  }

  private def handleRequestResponse(id: String): Unit = {
    outstandingRequests -= id
    processQueuedRequests()
  }

  private def processQueuedRequests(): Unit = {
    queuedRequests.dequeueFirst(_ => true).foreach(tup => {
      sendQueuedRequest(tup._1, tup._2)
    })
  }

  override def receive: Receive = {
    case DevToolsConnected =>
      val devToolsRef = sender()
      log.debug(s"New Developer Tools client: $devToolsRef")
      log.info("A Developer Tools client connected!")
      currentDevToolsRef = Some(devToolsRef)
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
    case DevToolsDisconnected if currentDevToolsRef.contains(sender()) =>
      log.info("Developer Tools client disconnected")

      // The client actor should know about this itself, but disconnecting it here results in more consistent code.
      disconnectDevTools(Status.Success(Unit))

      if (domains.isEmpty) {
        context.become(receive)
      } else {
        context.become(waitingForDomainActorsToDie)
        // Stop any domain actors - we'll get fresh ones later on
        stopDomainActors()
      }
    case DevToolsDisconnected =>
      log.warn("Ignoring disconnect signal from unknown dev tools sender")

    case FromDevTools(msg) if currentDevToolsRef.contains(sender()) =>
      log.trace(s"Incoming message from Developer Tools: $msg")
      handleIncomingMessage(msg)

    case FromDevTools(msg) =>
      log.warn(s"Ignoring message ($msg) from unknown dev tools sender")

    case response: Messages.Accepted =>
      log.trace("Got accepted-response (no response data) from domain: " + response)
      sendToDevTools(Protocol.EmptyResponse(response.id.toLong))
      handleRequestResponse(response.id)

    case response: Messages.Response =>
      log.trace("Got response from domain: " + response)
      sendToDevTools(Protocol.Response(response.id.toLong, response.msg))
      handleRequestResponse(response.id)

    case response: Messages.ErrorResponse =>
      log.warn("Got error response from domain: " + response)
      sendToDevTools(Protocol.ErrorResponse(response.id.toLong, response.error))
      handleRequestResponse(response.id)

    case event: Messages.Event =>
      log.trace("Got event from domain: " + event)
      sendToDevTools(Protocol.Event(event.method, event.params))

    case DevToolsConnected =>
      val devToolsRef = sender()
      log.debug(s"New Developer Tools client: $devToolsRef")
      log.info("A new Developer Tools client connected!")

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
  private val handler: ActorRef = system.actorOf(Props(new DevToolsHandler(domainFactory)))

  def create(): ChromeServer = new ChromeServerImpl

  private class ChromeServerImpl extends ChromeServer {

    private val subject = new SerializedSubject[Protocol.Message]

    // Track connection status in order to not bug DevToolsHandler when we're not connected (though it
    // shouldn't matter, because it shouldn't listen to us at that point).
    private val isConnected = new AtomicBoolean()

    // Make sure the isConnected flag is updated when DevToolsHandler disconnects us.
    subject.subscribe(new Observer[Protocol.Message] {
      override def onNext(item: Message): Unit = {}
      override def onError(error: Throwable): Unit = isConnected.set(false)
      override def onComplete(): Unit = isConnected.set(false)
    })

    private val innerActor = system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case msg: Protocol.Message => subject.onNext(msg)
        case _: Status.Success =>
          // DevToolsHandler disconnected us!
          subject.onComplete()
          context.stop(self)
        case Status.Failure(t) =>
          // Not really used, implemented for the sake of completeness.
          subject.onError(t)
          context.stop(self)
      }
    }))

    override def disconnect() = {
      if (isConnected.compareAndSet(true, false)) {
        handler.tell(DevToolsDisconnected, innerActor)
        subject.onComplete()
      }
    }

    override def connect(): Observable[Protocol.Message] = {
      if (isConnected.compareAndSet(false, true)) {
        handler.tell(DevToolsConnected, innerActor)
      }
      subject
    }

    override def sendMessage(message: Protocol.IncomingMessage) =
      if (isConnected.get()) {
        handler.tell(FromDevTools(message), innerActor)
      }
  }
}
