package com.programmaticallyspeaking.ncd.nashorn

import akka.actor.{ActorSystem, TypedActor, TypedProps}
import com.programmaticallyspeaking.ncd.host.ScriptHost
import com.sun.jdi.event.EventQueue
import com.sun.jdi.{StackFrame => _}
import org.slf4s.Logging

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}

class NashornDebugger(hostName: String, port: Int) extends Logging {
  def start(): Future[NashornScriptHost] = {
    val promise = Promise[NashornScriptHost]()
    // TODO: Make this a daemon thread?
    new Thread(() => try connect(promise) catch {
      case ex: Exception => promise.tryFailure(ex)
    }).start()
    promise.future
  }

  private def initAndListen(host: NashornScriptHost): Unit = {
    host.initialize()
    new ListenerThread(host).start()
  }

  def activateAsActor(host: NashornScriptHost)(implicit system: ActorSystem): ScriptHost = {
    // Create a typed actor that ensures that all NashornScriptHost access happens inside an actor
    val scriptHostAsActor = TypedActor(system).typedActorOf(TypedProps(classOf[NashornScriptHost], host), "scriptHost")

    // Initialize and start listening *using the TypedActor wrapper*
    initAndListen(scriptHostAsActor)

    val scriptHostActorRef = TypedActor(system).getActorRefFor(scriptHostAsActor)
    log.info(s"ScriptHost actor is at ${scriptHostActorRef.path.toStringWithoutAddress}")
    scriptHostAsActor
  }

  private def connect(connectionPromise: Promise[NashornScriptHost]): Unit = {
    log.info(s"Connecting to $hostName:$port...")
    val vm = Connections.connect(hostName, port, 5000)
    val theHost = new NashornDebuggerHost(vm)
    log.info("Connected!")
    connectionPromise.success(theHost)
  }

  // TODO: Daemon thread?
  private class ListenerThread(host: NashornScriptHost) extends Thread {
    override def run(): Unit = {
      listenIndefinitely(host.virtualMachine.eventQueue())
    }

    @tailrec
    private def listenIndefinitely(queue: EventQueue): Unit = {
      Option(queue.remove(1000)).foreach(host.handleEventSet)
      listenIndefinitely(queue)
    }
  }
}