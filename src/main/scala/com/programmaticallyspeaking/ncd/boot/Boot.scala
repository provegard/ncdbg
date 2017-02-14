package com.programmaticallyspeaking.ncd.boot

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.programmaticallyspeaking.ncd.chrome.domains.DefaultDomainFactory
import com.programmaticallyspeaking.ncd.chrome.net.Webservice
import com.programmaticallyspeaking.ncd.host.ScriptEvent
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.nashorn.{NashornDebugger, NashornDebuggerConnector, NashornScriptHost}
import org.slf4s.Logging

import scala.util.{Failure, Success}

object Boot extends App with Logging {

  implicit val system = ActorSystem()
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  val connector = new NashornDebuggerConnector("localhost", 7777)
  val debuggerReady = connector.connect().map(vm => new NashornDebugger().create(vm))

  debuggerReady.andThen {
    case Success(host) =>
      startListening(host)
      startHttpServer()
    case Failure(t) =>
      log.error("Failed to start the debugger", t)
      system.terminate()
      die(1)
  }

  private def die(code: Int): Unit = {
    system.terminate()
    System.exit(code)
  }

  private def startListening(host: NashornScriptHost) = {
    host.events.subscribe(new Observer[ScriptEvent] {
      override def onNext(item: ScriptEvent): Unit = {}

      override def onError(error: Throwable): Unit = {
        log.error("Exiting due to an error", error)
        die(2)
      }

      override def onComplete(): Unit = {
        log.info("Exiting because the debug target disconnected")
        die(0)
      }
    })
  }

  private def startHttpServer(): Unit = {
    val service = new Webservice(new DefaultDomainFactory())

    val binding = Http().bindAndHandle(service.route, "localhost", 7778)
    binding.onComplete {
      case Success(b) =>
        val localAddress = b.localAddress
        log.info(s"Server is listening on ${localAddress.getHostName}:${localAddress.getPort}")
        val url = s"chrome-devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=${localAddress.getHostName}:${localAddress.getPort}/dbg"
        log.info(url)
      case Failure(e) =>
        log.error("Binding failed", e)
        die(2)
    }
  }
}
