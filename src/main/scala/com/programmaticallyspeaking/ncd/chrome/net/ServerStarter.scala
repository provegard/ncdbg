package com.programmaticallyspeaking.ncd.chrome.net

import scala.util.{Failure, Random, Success, Try}

trait ServerStarter[TServer] {

  def startServer(port: Int): TServer

  def startServer(): (TServer, Int) = {
    val r = new Random()
    var lastFailure: Throwable = null
    for (i <- 1 to 20) {
      val port = 50000 + r.nextInt(5000)
      Try(startServer(port)) match {
        case Success(server) => return (server, port)
        case Failure(t) => lastFailure = t
      }
    }
    throw new RuntimeException("Failed to start the server", lastFailure)
  }
}
