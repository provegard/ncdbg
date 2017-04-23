package com.programmaticallyspeaking.ncd.nashorn

import java.util.concurrent.Executors

import akka.actor.ActorSystem
import com.programmaticallyspeaking.ncd.infra.ExecutorProxy
import com.sun.jdi.event.EventQueue
import com.sun.jdi.{VMDisconnectedException, VirtualMachine}
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
    log.info("VM information: " + vm.description())

    connectionPromise.success(vm)
  }
}

class NashornDebugger(implicit executionContext: ExecutionContext) extends Logging {

  private val hostExecutor = Executors.newSingleThreadExecutor()

  private def initAndListen(host: NashornScriptHost): Unit = {
    // TODO: Do we want to react on init success/failure here? Probably...
    new NashornScriptHostInteractionThread(host, Promise[Unit]).start()
  }

  def create(virtualMachine: VirtualMachine)(implicit system: ActorSystem): NashornScriptHost = {
    var singleThreadedScriptHost: NashornScriptHost = null

    def asyncInvokeOnHost(invoker: (NashornScriptHost => Any)): Future[Any] = {
      Future(invoker(singleThreadedScriptHost))
    }

    val theHost = new NashornDebuggerHost(virtualMachine, asyncInvokeOnHost)

    // Create a host proxy that ensures that all NashornScriptHost access happens on a single thread
    singleThreadedScriptHost = new ExecutorProxy(hostExecutor).createFor[NashornScriptHost](theHost)

    // Initialize and start listening *using the single-threaded proxy*
    initAndListen(singleThreadedScriptHost)

    singleThreadedScriptHost
  }
}

// TODO: Daemon thread?
class NashornScriptHostInteractionThread(host: NashornScriptHost, initPromise: Promise[Unit]) extends Thread with Logging {
  override def run(): Unit = {
    try {
      host.initialize()
      initPromise.trySuccess(())
      listenIndefinitely(host.virtualMachine.eventQueue())
    } catch {
      case ex: VMDisconnectedException =>
        log.info("Virtual machine disconnected")
        initPromise.tryFailure(ex)
      case ex: Exception =>
        log.error("VM interaction failure", ex)
        initPromise.tryFailure(ex)
    }
  }

  @tailrec
  private def listenIndefinitely(queue: EventQueue): Unit = {
    Option(queue.remove(1000)).foreach { es => host.handleOperation(NashornEventSet(es)) }
    listenIndefinitely(queue)
  }
}
