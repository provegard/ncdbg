package com.programmaticallyspeaking.ncd.boot

import java.util.logging.LogManager

import akka.actor.ActorSystem
import com.programmaticallyspeaking.ncd.infra.BuildProperties
import com.programmaticallyspeaking.ncd.nashorn.AttachingHostProxy
import org.slf4s.Logging

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object Boot extends App with Logging {
  val conf = new Conf(args)

  implicit val system = ActorSystem()
  import scala.concurrent.ExecutionContext.Implicits._

  // Disable java.util logging (used by the Closure Compiler)
  LogManager.getLogManager.reset()

  log.info("NCDbg version: " + BuildProperties.version)
  log.info("Local Java version: " + System.getProperty("java.version"))

  val attachBehavior = false

  val broker = new Broker(conf)

  val futureHost = if (attachBehavior) {
    log.info("Starting in attach mode. Will attach to the debug target upon a DevTools connection.")
    val proxy = new AttachingHostProxy(broker, 10.seconds)
    Future.successful(proxy.createHost())
  } else {
    broker.connect({
      case Some(t) => die(2)
      case None => die(0)
    }).map(_.host)
  }

  futureHost.onComplete {
    case Success(host) =>
      val server = new Server(conf)
      try server.start(host) catch {
        case NonFatal(t) =>
          log.error("Failed to start server", t)
          die(3)
      }

    case Failure(t) =>
      log.error("Host creation failed", t)
      die(3)
  }

  private def die(code: Int): Nothing = {
    system.terminate()
    System.exit(code)
    ???
  }
}
