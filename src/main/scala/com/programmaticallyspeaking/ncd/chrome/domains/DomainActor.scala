package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.{Actor, ActorRef, Stash, TypedActor, TypedProps}
import akka.util.Timeout
import com.programmaticallyspeaking.ncd.host.{ScriptEvent, ScriptHost}
import com.programmaticallyspeaking.ncd.messaging.{Observer, Subscription}
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object DomainActor {
  private[DomainActor] case class ScriptHostRef(actorRef: ActorRef)
}

abstract class DomainActor extends Actor with Logging with Stash {
  import DomainActor._

  protected var scriptHost: ScriptHost = _
  private var scriptEventSubscription: Subscription = _

  private var isProcessingRequest = false
  private val eventsToEmit = ListBuffer[Messages.Event]()
  private var lastRequestor: ActorRef = _

  val name = getClass.getSimpleName // assume the implementing class is named after the domain

  override def preStart(): Unit = try super.preStart() finally {
    import context.dispatcher
    val scriptHostActor = context.actorSelection("/user/scriptHost")
    implicit val timeout = Timeout(1.second)
    scriptHostActor.resolveOne().onComplete {
      case Success(actorRef) => self ! ScriptHostRef(actorRef)
      case Failure(t) =>
        log.error("Failed to obtain the ScriptHost reference.", t)
        context.stop(self)
    }
  }

  override def postStop(): Unit = try {
    Option(scriptEventSubscription).foreach(_.unsubscribe())
  } finally super.postStop()

  private def setScriptHost(host: ScriptHost): Unit = {
    scriptHost = host

    // Subscribe to events from the host and pass them to our receive function
    scriptEventSubscription = scriptHost.events.subscribe(new Observer[ScriptEvent] {
      override def onNext(item: ScriptEvent): Unit = self ! item

      override def onError(error: Throwable): Unit = {
        log.error(s"[$name] Script event error, exiting")
        context.stop(self)
      }

      override def onComplete(): Unit = {
        log.info(s"[$name] Script event completion, exiting")
        context.stop(self)
      }
    })

    scriptHostReceived()
  }

  protected def scriptHostReceived(): Unit = {}

  override def receive: Receive = {
    case ScriptHostRef(actorRef) =>
      log.info(s"[$name] Obtained a ScriptHost reference")
      // TODO: Error handling!
      val host = TypedActor(context).typedActorOf(TypedProps[ScriptHost], actorRef)
      setScriptHost(host)

      unstashAll()

    case scriptEvent: ScriptEvent => // Ignore events when disabled

    case req: Messages.Request if scriptHost == null =>
      log.debug(s"[$name] Got a Request without a ScriptHost, stashing...")
      stash() //TODO: Test

    case req@Messages.Request(_, Domain.enable) =>
      log.info(s"Enabling domain $name")
      context.become(receiveEnabled)
      processRequest(req)

    case req: Messages.Request =>
      val err = Messages.ErrorResponse(req.id, s"Domain $name is not enabled")
      sender() ! err
  }

  def receiveEnabled: Receive = {
    case Messages.Request(id, Domain.enable) =>
      val err = Messages.ErrorResponse(id, s"Domain $name is already enabled")
      sender() ! err

    case req: Messages.Request =>
      processRequest(req)

    case scriptEvent: ScriptEvent =>
      log.debug(s"[$name] Got script event $scriptEvent")
      handleScriptEvent.applyOrElse(scriptEvent, { se: ScriptEvent =>
        log.debug(s"[$name] Dropping unhandled script event $scriptEvent")
      })
  }

  private def processRequest(req: Messages.Request): Unit = {
    // We need this when calling emitQueuedEvents outside of request processing
    lastRequestor = sender()

    isProcessingRequest = true
    try {
      handle.applyOrElse(req.msg, { x: AnyRef =>
        unhandledBySubclass(req, x) //.asInstanceOf[Any]
        //            Messages.ErrorResponse(req.id, "Method not supported").asInstanceOf[Any]
      }) match {
        case msg: Messages.DomainMessage =>
          lastRequestor ! msg
        case u: Unit =>
          lastRequestor ! Messages.Accepted(req.id)
        case data =>
          lastRequestor ! Messages.Response(req.id, data)
      }
    } catch {
      case ex: Exception =>
        log.error(s"Message handling error for domain $name", ex)
        lastRequestor ! Messages.ErrorResponse(req.id, ex.getMessage)
    } finally {
      isProcessingRequest = false
      emitQueuedEvents()

      //        if (req.msg == Domain.disable) isEnabled = false
      if (req.msg == Domain.disable) {
        log.info(s"Disabling domain $name")
        context.become(receive)
      }
    }

  }

  private def unhandledBySubclass(req: Messages.Request, x: AnyRef): Messages.DomainMessage = req.msg match {
    case Domain.enable|Domain.disable =>
      Messages.Accepted(req.id)
    case _ =>
      Messages.ErrorResponse(req.id, "Method not supported")
  }

  private def emitQueuedEvents(): Unit = {
    Option(lastRequestor) match {
      case Some(actorRef) =>
        eventsToEmit.foreach(actorRef !)
        eventsToEmit.clear()
      case None =>
        ??? // TODO
    }
  }

  protected def emitEvent(method: String, params: Any): Unit = {
    eventsToEmit += Messages.Event(method, params)
    if (!isProcessingRequest) emitQueuedEvents()
  }

  protected def handle: PartialFunction[AnyRef, Any] = PartialFunction.empty[AnyRef, Any]

  protected def handleScriptEvent: PartialFunction[ScriptEvent, Unit] = PartialFunction.empty[ScriptEvent, Unit]

}
