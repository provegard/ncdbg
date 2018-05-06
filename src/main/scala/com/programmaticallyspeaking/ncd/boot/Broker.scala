package com.programmaticallyspeaking.ncd.boot

import java.net.ConnectException

import akka.actor.ActorSystem
import com.programmaticallyspeaking.ncd.chrome.domains.EventEmitHook
import com.programmaticallyspeaking.ncd.chrome.net.FilePublisher
import com.programmaticallyspeaking.ncd.config.Conf
import com.programmaticallyspeaking.ncd.host.{ScriptEvent, ScriptHost}
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.nashorn.{NashornDebugger, NashornDebuggerConnector, NashornScriptHost}
import org.slf4s.Logging

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

case class BrokerConnection(host: NashornScriptHost, disconnect: () => Unit)

class Broker(conf: Conf)(implicit actorSystem: ActorSystem) extends Logging {
  import scala.concurrent.ExecutionContext.Implicits._

  def connect(errorCallback: Option[Throwable] => Unit): Future[BrokerConnection] = {
    val connectAddr = conf.connect()
    val connector = new NashornDebuggerConnector(connectAddr.host, connectAddr.port)
    val debuggerReady = connector.connect().map(vm => new NashornDebugger().create(vm))

    val connectionPromise = Promise[BrokerConnection]()

    debuggerReady.onComplete {
      case Success(host) =>
        startListening(host, errorCallback)

        try {
          def disconnect(): Unit = {
            host.virtualMachine.inner.dispose()
          }
          val conn = BrokerConnection(host, disconnect)
          connectionPromise.success(conn)
        } catch {
          case NonFatal(t) =>
            log.error("Binding failed", t)
            connectionPromise.failure(new RuntimeException("connection failed"))
        }
      case Failure(t) =>
        t match {
          case _: ConnectException =>
            log.error("Failed to connect to the debug target.")
            log.error("Please make sure that the debug target is started with debug VM arguments, for example:")
            log.error(s"  -Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=${connectAddr.host}:${connectAddr.port}")
          case _ =>
            log.error("Failed to start the debugger", t)
        }
        // Wrap in RuntimeException if needed, otherwise we'll get UndeclaredThrowableException wrapping the cause.
        val error = if (t.isInstanceOf[RuntimeException]) t else new RuntimeException(t)
        connectionPromise.failure(error)
    }

    connectionPromise.future
  }

  private def startListening(host: NashornScriptHost, errorCallback: Option[Throwable] => Unit) = {
    host.events.subscribe(new Observer[ScriptEvent] {
      override def onNext(item: ScriptEvent): Unit = {}

      override def onError(error: Throwable): Unit = {
        log.error("Unknown error", error)
        errorCallback(Some(error))
      }

      override def onComplete(): Unit = {
        log.info("The debug target disconnected")
        errorCallback(None)
      }
    })
  }
}

class BootContainer(filePublisher: FilePublisher, scriptHost: ScriptHost) extends Container(Seq(filePublisher, scriptHost, new EventEmitHook))
