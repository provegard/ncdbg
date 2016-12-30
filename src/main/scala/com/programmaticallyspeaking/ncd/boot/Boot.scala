package com.programmaticallyspeaking.ncd.boot

import akka.actor.{ActorSystem, TypedActor, TypedProps}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.programmaticallyspeaking.ncd.chrome.domains.DefaultDomainFactory
import com.programmaticallyspeaking.ncd.chrome.net.Webservice
import com.programmaticallyspeaking.ncd.host.ScriptHost
import com.programmaticallyspeaking.ncd.nashorn.NashornDebugger
import org.slf4s.Logging

import scala.util.{Failure, Success}

object Boot extends App with Logging {

  implicit val system = ActorSystem()
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  val debugger = new NashornDebugger("localhost", 7777)
  debugger.start().andThen {
    case Success(_) =>
      startHttpServer()
    case Failure(t) =>
      system.terminate()
      die(1)
  }

  private def die(code: Int): Unit = {
    system.terminate()
    System.exit(code)
  }

  private def startHttpServer(): Unit = {
    val scriptHostAsActor = TypedActor(system).typedActorOf(TypedProps(classOf[ScriptHost], debugger.scriptHost), "scriptHost")
    val scriptHostActorRef = TypedActor(system).getActorRefFor(scriptHostAsActor)
    log.info(s"ScriptHost actor is at ${scriptHostActorRef.path.toStringWithoutAddress}")

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
