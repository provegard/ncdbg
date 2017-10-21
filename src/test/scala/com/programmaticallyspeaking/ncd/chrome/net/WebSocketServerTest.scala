package com.programmaticallyspeaking.ncd.chrome.net

import java.net.URI
import java.util.concurrent.{ConcurrentHashMap, Executors}

import com.programmaticallyspeaking.ncd.host.{ScriptEvent, ScriptHost}
import com.programmaticallyspeaking.ncd.infra.{ExecutorProxy, ObjectMapping}
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.messaging.Subject
import com.programmaticallyspeaking.ncd.testing.{FakeFilePublisher, SharedInstanceActorTesting, UnitTest}
import org.java_websocket.drafts.Draft_17
import org.java_websocket.handshake.ServerHandshake
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}

class WebSocketServerTest extends UnitTest with BeforeAndAfterAll with MockitoSugar with ScalaFutures with SharedInstanceActorTesting with ServerStarter[WebSocketServer] {

  var domainFactory: CapturingDomainFactory = _

  var server: WebSocketServer = _
  var serverPort: Int = 0
  var wsClient: Client = _

  override def beforeAllTests(): Unit = try super.beforeAllTests() finally {
    val scriptHost = mock[ScriptHost]
    when(scriptHost.events).thenReturn(Subject.serialized[ScriptEvent])
    val scriptHostForContainer = new ExecutorProxy(Executors.newSingleThreadExecutor()).createFor[ScriptHost](scriptHost)
    implicit val container = new Container(Seq(FakeFilePublisher, scriptHostForContainer))
    domainFactory = new CapturingDomainFactory()
    server = new WebSocketServer(domainFactory, None)
  }

  override def startServer(port: Int): WebSocketServer = {
    server.start("localhost", port)
    server
  }

  override def beforeTest(): Unit = try super.beforeTest() finally {
    serverPort = startServer()._2
    wsClient = new Client
    if (!wsClient.connectBlocking()) throw new RuntimeException("Unable to connect to server")
  }

  override def afterTest(): Unit = try server.stop() finally super.afterTest()

  def toJson(data: Map[String, Any]) = ObjectMapping.toJson(data)


  private def enable(): Unit = {
    wsClient.sendMessage(toJson(Map("id" -> 1, "method" -> "FooTestDomain.enable", "params" -> null)))
    wsClient.expectMessage(toJson(Map("id" -> 1, "result" -> Map.empty)))
  }

  "the Chrome websocket service" - {
    "should respond to a message" in {
      enable()

      wsClient.sendMessage(toJson(Map("id" -> 2, "method" -> "FooTestDomain.bar", "params" -> null)))
      wsClient.expectMessage(toJson(Map("id" -> 2, "result" -> Map.empty)))
    }

    "should respond with data to a message" in {
      enable()

      wsClient.sendMessage(toJson(Map("id" -> 2, "method" -> "FooTestDomain.echo", "params" -> Map("msg" -> "hello"))))
      wsClient.expectMessage(toJson(Map("id" -> 2, "result" -> "hello")))
    }

    "should respond with an error for an _unhandled_ domain method" in {
      enable()

      wsClient.sendMessage(toJson(Map("id" -> 2, "method" -> "FooTestDomain.unhandled", "params" -> null)))
      wsClient.expectMessage(toJson(Map("id" -> 2, "error" -> "Method not supported")))
    }

    "should respond with an error for an unknown _domain_" in {
      wsClient.sendMessage(toJson(Map("id" -> 1, "method" -> "UnknownDomain.xyz", "params" -> null)))
      wsClient.expectMessage(toJson(Map("id" -> 1, "error" -> "Unknown domain or method: UnknownDomain.xyz")))
    }

    "should respond with an error for an unknown domain _method_" in {
      enable()

      wsClient.sendMessage(toJson(Map("id" -> 2, "method" -> "FooTestDomain.unknown", "params" -> null)))
      wsClient.expectMessage(toJson(Map("id" -> 2, "error" -> "Unknown domain or method: FooTestDomain.unknown")))
    }
  }

  class Client extends org.java_websocket.client.WebSocketClient(new URI(s"ws://localhost:$serverPort/dbg"), new Draft_17) {
    private val messagePromises = TrieMap[String, Promise[Unit]]()
    private val outstandingMessages = ConcurrentHashMap.newKeySet[String]()

    override def onError(ex: Exception): Unit = {}

    override def onMessage(message: String): Unit = {
      messagePromises.remove(message) match {
        case Some(p) => p.success(())
        case None => outstandingMessages.add(message)
      }
    }

    override def onClose(code: Int, reason: String, remote: Boolean): Unit = {}

    override def onOpen(handshakedata: ServerHandshake): Unit = {}

    def sendMessage(s: String): Unit = {
      outstandingMessages.clear() // remove old messages
      send(s)
    }

    def expectMessage(msg: String): Unit = {
      // Check if we have seen the message prior to this call.
      if (outstandingMessages.remove(msg)) return

      // We haven't seen the message, so wait for it.
      val p = Promise[Unit]()
      messagePromises += msg -> p
      Await.result(p.future, 3.seconds)
    }
  }
}
