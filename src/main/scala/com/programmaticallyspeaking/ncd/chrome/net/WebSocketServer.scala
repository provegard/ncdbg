package com.programmaticallyspeaking.ncd.chrome.net

import java.net.InetAddress
import java.util.concurrent.Executors

import akka.actor.ActorSystem
import com.programmaticallyspeaking.ncd.chrome.domains.DomainFactory
import com.programmaticallyspeaking.ncd.chrome.net.Protocol.Message
import com.programmaticallyspeaking.ncd.infra.ObjectMapping.{fromJson, toJson}
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.tinyws.Server
import com.programmaticallyspeaking.tinyws.Server.{LogLevel, WebSocketClient}
import org.slf4s.Logging

class WebSocketServer(domainFactory: DomainFactory)(implicit system: ActorSystem) extends Logging {

  private val chromeServerFactory = new ChromeServerFactory(domainFactory)
  private val executor = Executors.newCachedThreadPool()
  private var server: Server = _

  def start(host: String, port: Int): Unit = {
    val addr = InetAddress.getByName(host)
    server = new Server(executor, Server.Options.withPort(port).andAddress(addr).andLogger(new TinyWSLogger))
    server.addHandlerFactory("/dbg", () => new Handler)
    server.start()
  }

  def stop(): Unit = {
    Option(server).foreach(_.stop())
    server = null
  }

  class Handler extends Server.WebSocketHandler with Logging {
    private var theClient: WebSocketClient = _
    private var chromeServer: Option[ChromeServer] = None

    override def onOpened(client: WebSocketClient): Unit = {
      theClient = client
      val cs = chromeServerFactory.create()
      cs.connect().subscribe(new Observer[Message] {
        override def onError(error: Throwable) = {
          log.error("Closing due to error", error)
          theClient.close()
        }

        override def onComplete() = {
          log.debug("Disconnecting client")
          theClient.close()
        }

        override def onNext(item: Message) = {
          val json = toJson(item)
          log.trace(s"Sending over websocket: $json")
          theClient.sendTextMessage(json)
        }
      })
      chromeServer = Some(cs)
    }

    override def onClosedByClient(code: Int, reason: String): Unit = {
      log.trace(s"DevTools client closed the connection: $code ($reason)")
      chromeServer.foreach(_.disconnect())
    }

    override def onFailure(t: Throwable): Unit = {
      log.error("WebSocket error", t)
      chromeServer.foreach(_.disconnect())
    }

    override def onTextMessage(text: CharSequence): Unit = {
      val msg = fromJson[Protocol.IncomingMessage](text.toString)
      chromeServer.foreach(_.sendMessage(msg))
    }

    override def onBinaryData(data: Array[Byte]): Unit = {
      log.warn("Binary data ignored!")
    }

    override def onClosedByServer(code: Int, reason: String): Unit = {}
  }

  class TinyWSLogger extends Server.Logger {
    override def log(level: LogLevel, message: String, error: Throwable): Unit = level match {
      case LogLevel.INFO => WebSocketServer.this.log.info(message, error)
      case LogLevel.WARN => WebSocketServer.this.log.warn(message, error)
      case LogLevel.ERROR => WebSocketServer.this.log.error(message, error)
      case _ => // don't bother with trace or debug logging
    }

    override def isEnabledAt(level: LogLevel): Boolean = level match {
      case LogLevel.INFO => WebSocketServer.this.log.underlying.isInfoEnabled
      case LogLevel.WARN => WebSocketServer.this.log.underlying.isWarnEnabled
      case LogLevel.ERROR => WebSocketServer.this.log.underlying.isErrorEnabled
      case _ => false // don't bother with trace or debug logging
    }
  }
}
