package com.programmaticallyspeaking.ncd.nashorn

import akka.actor.{ActorSystem, TypedActor, TypedProps}
import com.sun.jdi.event.EventQueue
import com.sun.jdi.{VirtualMachine, StackFrame => _}
import org.slf4s.Logging

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}

class NashornDebuggerConnector(hostName: String, port: Int) extends Logging {
  def connect(): Future[VirtualMachine] = {
    val promise = Promise[VirtualMachine]()
    // TODO: Make this a daemon thread?
    new Thread(() => try connect(promise) catch {
      case ex: Exception => promise.tryFailure(ex)
    }).start()
    promise.future
  }

  private def connect(connectionPromise: Promise[VirtualMachine]): Unit = {
    log.info(s"Connecting to $hostName:$port...")
    val vm = Connections.connect(hostName, port, 5000)
    log.info("Connected!")
    connectionPromise.success(vm)
  }
}

class NashornDebugger(implicit executionContext: ExecutionContext) extends Logging {

//  private var typedActorWrappedHost: NashornScriptHost = _

//  def start(): Future[NashornScriptHost] = {
//    val promise = Promise[NashornScriptHost]()
//    // TODO: Make this a daemon thread?
//    new Thread(() => try connect(promise) catch {
//      case ex: Exception => promise.tryFailure(ex)
//    }).start()
//    promise.future
//  }

  private def initAndListen(host: NashornScriptHost): Unit = {
    // TODO: Do we want to react on init success/failure here? Probably...
    new NashornScriptHostInteractionThread(host, Promise[Unit]).start()
  }

  def create(virtualMachine: VirtualMachine)(implicit system: ActorSystem): NashornScriptHost = {
    var scriptHostAsActor: NashornScriptHost = null

    def asyncInvokeOnHost(invoker: (NashornScriptHost => Unit)): Unit = {
      Future(invoker(scriptHostAsActor))
    }

    val theHost = new NashornDebuggerHost(virtualMachine, asyncInvokeOnHost)

    // Create a typed actor that ensures that all NashornScriptHost access happens inside an actor
    scriptHostAsActor = TypedActor(system).typedActorOf(TypedProps(classOf[NashornScriptHost], theHost), "scriptHost")

    // Save for use in asyncInvokeOnHost
//    typedActorWrappedHost = scriptHostAsActor

    // Initialize and start listening *using the TypedActor wrapper*
    initAndListen(scriptHostAsActor)

    val scriptHostActorRef = TypedActor(system).getActorRefFor(scriptHostAsActor)
    log.info(s"ScriptHost actor is at ${scriptHostActorRef.path.toStringWithoutAddress}")
    scriptHostAsActor
  }

//  private def connect(connectionPromise: Promise[NashornScriptHost]): Unit = {
//    log.info(s"Connecting to $hostName:$port...")
//    val vm = Connections.connect(hostName, port, 5000)
//    val theHost = new NashornDebuggerHost(vm, asyncInvokeOnHost)
//    log.info("Connected!")
//    connectionPromise.success(theHost)
//  }
}

// TODO: Daemon thread?
class NashornScriptHostInteractionThread(host: NashornScriptHost, initPromise: Promise[Unit]) extends Thread with Logging {
  override def run(): Unit = {
    try {
      host.initialize()
      initPromise.trySuccess()
      listenIndefinitely(host.virtualMachine.eventQueue())
    } catch {
      case ex: Exception =>
        initPromise.tryFailure(ex)
        log.error("Interaction failure", ex)
    }
  }

  @tailrec
  private def listenIndefinitely(queue: EventQueue): Unit = {
    Option(queue.remove(1000)).foreach { es => host.handleOperation(NashornEventSet(es)) }
    listenIndefinitely(queue)
  }
}
