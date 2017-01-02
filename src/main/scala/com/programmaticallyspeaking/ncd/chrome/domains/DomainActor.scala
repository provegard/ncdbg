package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.{Actor, ActorRef, PoisonPill, Stash, Status, TypedActor, TypedProps}
import akka.util.Timeout
import com.programmaticallyspeaking.ncd.host.{Done, ScriptEvent, ScriptHost}
import com.programmaticallyspeaking.ncd.messaging.{Observer, Subscription}
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object DomainActor {
  private[DomainActor] case class ScriptHostRef(actorRef: ActorRef)

  private[DomainActor] case class ProcessingResult(req: Messages.Request, result: Any)
  private[DomainActor] case class ProcessingError(req: Messages.Request, t: Throwable)

  private[DomainActor] case class EmittableEvent(event: Messages.Event, receiver: ActorRef)
}

abstract class DomainActor extends Actor with Logging with Stash {
  import DomainActor._
  implicit val ec = ExecutionContext.global

  protected var scriptHost: ScriptHost = _
  private var scriptEventSubscription: Subscription = _

  private var isProcessingRequest = false
  private val eventsToEmit = ListBuffer[EmittableEvent]()
  private var lastRequestor: ActorRef = _

  val name = getClass.getSimpleName // assume the implementing class is named after the domain

  override def preStart(): Unit = try super.preStart() finally {
    val scriptHostActor = context.actorSelection("/user/scriptHost")
    implicit val timeout = Timeout(1.second)
    scriptHostActor.resolveOne().onComplete {
      case Success(actorRef) => self ! ScriptHostRef(actorRef)
      case Failure(t) =>
        log.error("Failed to obtain the ScriptHost reference.", t)
        self ! PoisonPill // context.stop doesn't work here, NPE
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

    case ProcessingResult(req, result) =>
      result match {
        case msg: Messages.DomainMessage =>
          sender() ! msg
        case u: Unit =>
          sender() ! Messages.Accepted(req.id)
        case d: Done =>
          // Done is a Unit replacement to achieve synchronous calls, so we treat it just as Unit.
          sender() ! Messages.Accepted(req.id)
        case data =>
          sender() ! Messages.Response(req.id, data)
      }
      finishProcessingRequest(req)

    case ProcessingError(req, ex) =>
      log.error(s"Message handling error for domain $name", ex)
      sender() ! Messages.ErrorResponse(req.id, ex.getMessage)
      finishProcessingRequest(req)
  }

  private def finishProcessingRequest(req: Messages.Request): Unit = {
    isProcessingRequest = false
    emitQueuedEvents()

    if (req.msg == Domain.disable) {
      log.info(s"Disabling domain $name")
      context.become(receive)
    }
  }


  private def handleProcessingResult(req: Messages.Request, theSender: ActorRef, t: Try[Any]): Unit = t match {
    case Success(f: Future[_]) => f.onComplete(handleProcessingResult(req, theSender, _: Try[Any]))
    case Success(result) => self.tell(ProcessingResult(req, result), theSender)
    case Failure(ex) => self.tell(ProcessingError(req, ex), theSender)
  }

  private def processRequest(req: Messages.Request): Unit = {
    // We need this when calling emitQueuedEvents outside of request processing
    lastRequestor = sender()
    isProcessingRequest = true

    val unhandled = unhandledBySubclass(req, _: AnyRef)
    handleProcessingResult(req, lastRequestor, Try(handle.applyOrElse(req.msg, unhandled)))
  }

  private def unhandledBySubclass(req: Messages.Request, x: AnyRef): Messages.DomainMessage = req.msg match {
    case Domain.enable|Domain.disable =>
      Messages.Accepted(req.id)
    case _ =>
      Messages.ErrorResponse(req.id, "Method not supported")
  }

  private def emitQueuedEvents(): Unit = {
    eventsToEmit.foreach { e =>
      e.receiver ! e.event
    }
    eventsToEmit.clear()
  }

  protected def emitEvent(method: String, params: Any): Unit = {
    assert(lastRequestor != null, "lastRequester must be set in emitEvent")
    eventsToEmit += EmittableEvent(Messages.Event(method, params), lastRequestor)
    if (!isProcessingRequest) emitQueuedEvents()
  }

  protected def handle: PartialFunction[AnyRef, Any] = PartialFunction.empty[AnyRef, Any]

  protected def handleScriptEvent: PartialFunction[ScriptEvent, Unit] = PartialFunction.empty[ScriptEvent, Unit]

}
