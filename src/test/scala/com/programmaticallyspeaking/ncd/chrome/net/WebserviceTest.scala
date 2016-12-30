package com.programmaticallyspeaking.ncd.chrome.net

import akka.actor.{TypedActor, TypedProps}
import akka.http.scaladsl.testkit.{ScalatestRouteTest, WSProbe}
import com.programmaticallyspeaking.ncd.host.{ScriptEvent, ScriptHost}
import com.programmaticallyspeaking.ncd.infra.ObjectMapping
import com.programmaticallyspeaking.ncd.messaging.Subject
import com.programmaticallyspeaking.ncd.testing.{NoActorLogging, UnitTest}
import com.typesafe.config.Config
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class WebserviceTest extends UnitTest with ScalatestRouteTest with BeforeAndAfterEach with BeforeAndAfterAll with MockitoSugar {

  lazy val domainFactory = new CapturingDomainFactory()

  var wsClient: WSProbe = _  // = WSProbe()
  val service = new Webservice(domainFactory)


  override def testConfig: Config = NoActorLogging.config

  override protected def beforeAll(): Unit = try super.beforeAll() finally {
    val scriptHost = mock[ScriptHost]
    when(scriptHost.events).thenReturn(Subject.serialized[ScriptEvent])
    TypedActor(system).typedActorOf(TypedProps(classOf[ScriptHost], scriptHost), "scriptHost")
  }

  override protected def beforeEach(): Unit = try super.beforeEach() finally {
    wsClient = WSProbe()
  }

  def toJson(data: Map[String, Any]) = ObjectMapping.toJson(data)

  def testWS[Unit](fun: => Unit) = check {
    // check response for WS Upgrade headers
    isWebSocketUpgrade shouldEqual true

    fun

    wsClient.sendCompletion()
    wsClient.expectCompletion()
  }

  private def enable(): Unit = {
    wsClient.sendMessage(toJson(Map("id" -> "1", "method" -> "FooTestDomain.enable", "params" -> null)))
    wsClient.expectMessage(toJson(Map("id" -> "1")))
  }

  "the Chrome websocket service" - {
    "should respond to a message" in {
      // WS creates a WebSocket request for testing
      WS("/dbg", wsClient.flow) ~> service.route ~>
        testWS {
          enable()

          wsClient.sendMessage(toJson(Map("id" -> "2", "method" -> "FooTestDomain.bar", "params" -> null)))
          wsClient.expectMessage(toJson(Map("id" -> "2")))
        }
    }

    "should respond with data to a message" in {
      // WS creates a WebSocket request for testing
      WS("/dbg", wsClient.flow) ~> service.route ~>
        testWS {
          enable()

          wsClient.sendMessage(toJson(Map("id" -> "2", "method" -> "FooTestDomain.echo", "params" -> Map("msg" -> "hello"))))
          wsClient.expectMessage(toJson(Map("id" -> "2", "result" -> "hello")))
        }
    }

    "should respond with an error for an _unhandled_ domain method" in {
      WS("/dbg", wsClient.flow) ~> service.route ~>
        testWS {
          enable()

          wsClient.sendMessage(toJson(Map("id" -> "2", "method" -> "FooTestDomain.unhandled", "params" -> null)))
          wsClient.expectMessage(toJson(Map("id" -> "2", "error" -> "Method not supported")))
        }
    }

    "should respond with an error for an unknown _domain_" in {
      WS("/dbg", wsClient.flow) ~> service.route ~>
        testWS {
          wsClient.sendMessage(toJson(Map("id" -> "1", "method" -> "UnknownDomain.xyz", "params" -> null)))
          wsClient.expectMessage(toJson(Map("id" -> "1", "error" -> "Unknown domain or method: UnknownDomain.xyz")))
        }
    }

    "should respond with an error for an unknown domain _method_" in {
      WS("/dbg", wsClient.flow) ~> service.route ~>
        testWS {
          enable()

          wsClient.sendMessage(toJson(Map("id" -> "2", "method" -> "FooTestDomain.unknown", "params" -> null)))
          wsClient.expectMessage(toJson(Map("id" -> "2", "error" -> "Unknown domain or method: FooTestDomain.unknown")))
        }
    }
  }
}
