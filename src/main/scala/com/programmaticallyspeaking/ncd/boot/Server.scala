package com.programmaticallyspeaking.ncd.boot

import akka.actor.ActorSystem
import com.programmaticallyspeaking.ncd.chrome.domains.DefaultDomainFactory
import com.programmaticallyspeaking.ncd.chrome.net.{FileServer, WebSocketServer}
import com.programmaticallyspeaking.ncd.config.Conf
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.nashorn.NashornScriptHost
import org.slf4s.Logging

case object ServerStarted

class Server(conf: Conf)(implicit actorSystem: ActorSystem) extends Logging {
  def start(host: NashornScriptHost): Unit = {
    val listenAddr = conf.listen()
    val fileServer = new FileServer(listenAddr.host, listenAddr.port)
    val container = new BootContainer(fileServer.publisher, host)
    startHttpServer(container, fileServer)
  }

  private def startHttpServer(container: Container, fileServer: FileServer): Unit = {
    val server = new WebSocketServer(new DefaultDomainFactory(container), Some(fileServer))
    val listenAddr = conf.listen()
    server.start(listenAddr.host, listenAddr.port)
    log.info(s"Server is listening on ${listenAddr.host}:${listenAddr.port}")
    val url = s"devtools://devtools/bundled/inspector.html?ws=${listenAddr.host}:${listenAddr.port}/dbg"
    log.info("Open this URL in Chrome: " + url)
  }
}
