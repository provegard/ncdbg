package com.programmaticallyspeaking.ncd.boot

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.programmaticallyspeaking.ncd.chrome.domains.DefaultDomainFactory
import com.programmaticallyspeaking.ncd.chrome.net.Webservice
import com.programmaticallyspeaking.ncd.nashorn.{NashornDebugger, NashornScriptHost}
import org.slf4s.Logging

import scala.util.{Failure, Success}

object Boot extends App with Logging {

  implicit val system = ActorSystem()
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  val debugger = new NashornDebugger("localhost", 7777)

  debugger.start().andThen {
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
    debugger.activateAsActor(host)
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
