package com.programmaticallyspeaking.ncd.nashorn

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method, Proxy}

import com.programmaticallyspeaking.ncd.host.ScriptHost
import com.sun.jdi.{StackFrame => _, _}
import org.slf4s.Logging

import scala.concurrent.{Future, Promise}

class NashornDebugger(hostName: String, port: Int) extends Logging {
  private var debuggerHost: Option[NashornDebuggerHost] = None

  lazy val scriptHost = createProxy()

  def start(): Future[Unit] = {
    val promise = Promise[Unit]()
    // TODO: Make this a daemon thread?
    new Thread(new Runnable {


      override def run(): Unit = try connectAndListen(promise) catch {
        case ex: Exception =>
          log.error("Debugger failure", ex)
          cleanup()
          promise.tryFailure(ex)
      }
    }).start()
    promise.future
  }

  private def cleanup() = try {
    debuggerHost.foreach(h => h.virtualMachine.dispose())
    debuggerHost = None
  } catch {
    case ex: Exception =>
      log.error("Ignoring error during cleanup", ex)
  }

  private def connectAndListen(connectionPromise: Promise[Unit]): Unit = {
    log.info(s"Connecting to $hostName:$port...")
    val vm = Connections.connect(hostName, port, 5000)
    val theHost = new NashornDebuggerHost(vm)
    debuggerHost = Some(theHost)
    log.info("Connected!")
    connectionPromise.success(())

    // Will block
    theHost.startListening()
  }

  private def createProxy(): ScriptHost = {
    Proxy.newProxyInstance(getClass.getClassLoader, Array(classOf[ScriptHost]), new ScriptHostProxy).asInstanceOf[ScriptHost]
  }

  class ScriptHostProxy extends InvocationHandler {
    override def invoke(proxy: scala.Any, method: Method, args: Array[AnyRef]): AnyRef = {
      debuggerHost match {
        case Some(host) =>

          try {
            method.invoke(host, args: _*)
          } catch {
            case ex: InvocationTargetException =>
              throw ex.getTargetException
          }

        case None =>
          throw new IllegalStateException("The virtual machine is not connected.")
      }
    }
  }
}