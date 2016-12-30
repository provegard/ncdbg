package com.programmaticallyspeaking.ncd.chrome.net

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import akka.stream.stage._
import com.programmaticallyspeaking.ncd.chrome.domains.DomainFactory
import com.programmaticallyspeaking.ncd.infra.ObjectMapping._
import org.slf4s.Logging

class Webservice(domainFactory: DomainFactory)(implicit fm: Materializer, system: ActorSystem) extends Directives with Logging {

  val chromeServerFactory = new ChromeServerFactory(domainFactory)

  def route =
    get {
      path("dbg") {
        handleWebSocketMessages(websocketDebugFlow())
      }
    }

  def websocketDebugFlow(): Flow[Message, Message, Any] = {
    log.info("Got a websocket debug connection.")
    val chromeServer = chromeServerFactory.create()
    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) => msg
        // FIXME: We need to handle TextMessage.Streamed as well.
        case TextMessage.Streamed(src) => ???
      }
      .via(chromeServer.messageFlow()) // ... and route them through the chatFlow ...
      .map { msg: Protocol.Message =>
        val json = toJson(msg)
        log.trace(s"Sending over websocket: $json")
        TextMessage.Strict(json) // ... pack outgoing messages into WS JSON messages ...
      }
      .via(reportErrorsFlow) // ... then log any processing errors
  }

  def reportErrorsFlow[T]: Flow[T, T, Any] =
    Flow[T]
      .transform(() => new PushStage[T, T] {
        def onPush(elem: T, ctx: Context[T]): SyncDirective = ctx.push(elem)

        override def onUpstreamFailure(cause: Throwable, ctx: Context[T]): TerminationDirective = {
          log.error("WS stream failed", cause)
          super.onUpstreamFailure(cause, ctx)
        }
      })
}
