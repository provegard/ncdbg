package com.programmaticallyspeaking.ncd.chrome.net

import akka.actor.PoisonPill
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.stream.testkit.{TestPublisher, TestSubscriber}
import akka.testkit.TestProbe
import com.programmaticallyspeaking.ncd.chrome.domains.{DomainActorTesting, FooTestDomain}
import com.programmaticallyspeaking.ncd.chrome.net.Protocol.{EmptyResponse, ErrorResponse, Message, Response}
import com.programmaticallyspeaking.ncd.host.ScriptEvent
import com.programmaticallyspeaking.ncd.infra.ObjectMapping.fromJson
import com.programmaticallyspeaking.ncd.messaging.{Observable, Observer}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.reactivestreams.{Publisher, Subscriber, Subscription}
import org.scalatest.Inside
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.language.implicitConversions

object AkkaTestMigration {
  implicit def observable2Publisher[A](observable: Observable[A]): Publisher[A] = (s: Subscriber[_ >: A]) => {
    val subscription = observable.subscribe(new Observer[A] {
      override def onError(error: Throwable): Unit = s.onError(error)

      override def onComplete(): Unit = s.onComplete()

      override def onNext(item: A): Unit = s.onNext(item)
    })
    s.onSubscribe(new Subscription {
      override def cancel(): Unit = subscription.unsubscribe()

      override def request(n: Long): Unit = { /* not implemented */ }
    })
  }
}

class ChromeServerTest extends UnitTest with DomainActorTesting with Inside with TableDrivenPropertyChecks {
  import AkkaTestMigration._
  import org.mockito.Mockito._
  lazy val domainFactory = new CapturingDomainFactory()
  lazy implicit val materializer = ActorMaterializer()
  lazy val chromeServerFactory = new ChromeServerFactory(domainFactory)

  def setup(server: ChromeServer): (TestPublisher.Probe[String], TestSubscriber.Probe[Any]) = {
    val source: Source[Message, Any] = Source.fromPublisher(server.connect())
    val sink: Sink[String, Any] = Sink.foreach(s => server.sendMessage(fromJson[Protocol.IncomingMessage](s)))

    val flowUnderTest: Flow[String, Message, Any] = Flow.fromSinkAndSource(sink, source)

    TestSource.probe[String]
      .via(flowUnderTest)
      .toMat(TestSink.probe[Any])(Keep.both)
      .run()
  }

  def newConnection(f: (TestPublisher.Probe[String], TestSubscriber.Probe[Any]) => Unit) = {
    val (pub, sub) = setup(chromeServerFactory.create())
    f(pub, sub)
  }


  def enableDomain(pub: TestPublisher.Probe[String], sub: TestSubscriber.Probe[Any]): Unit = {
    sub.request(1)
    pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
    sub.expectNext(EmptyResponse(1))
  }

  "ChromeServer" - {
    "should re-create an actor after it has failed in preStart a first time" in {
      var call = 0
      when(currentScriptHost.events).thenAnswer(new Answer[Observable[ScriptEvent]] {
        override def answer(invocation: InvocationOnMock): Observable[ScriptEvent] = {
          call += 1
          if (call == 1) throw new RuntimeException("fail")
          else scriptEventSubject
        }
      })

      newConnection {
        case (pub, sub) =>
          sub.request(1)
          pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
          sub.expectComplete()
      }

      newConnection {
        case (pub, sub) =>
          sub.request(1)
          pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
          sub.expectNext(EmptyResponse(1))
      }
    }

    "should setup a DevTools handler to respond to messages" in {
      val (pub, sub) = setup(chromeServerFactory.create())

      sub.request(1)
      pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
      sub.expectNext(EmptyResponse(1))
    }

    "should stop domain actors if a second DevTools client connects" in {
      (enableDomain _).tupled(setup(chromeServerFactory.create()))

      domainFactory.actorByName("FooTestDomain") match {
        case Some(domainActor) =>
          val probe = TestProbe()
          probe.watch(domainActor)

          // Second client connects
          (enableDomain _).tupled(setup(chromeServerFactory.create()))

          probe.expectTerminated(domainActor)

        case None => fail("FooTestDomain not created")
      }
    }

    "should not create new domain actors before the old ones are gone, to ensure that there is one set of domain actors at a time" in {
      domainFactory.requireNoOldActor() // this is the magic of this test

      (enableDomain _).tupled(setup(chromeServerFactory.create()))

      domainFactory.actorByName("FooTestDomain") match {
        case Some(domainActor1) =>
          // Second client connects
          (enableDomain _).tupled(setup(chromeServerFactory.create()))

        case None => fail("FooTestDomain not created")
      }
    }

    "should disconnect a DevTools client if the last domain actor stops" in {
      val (pub, sub) = setup(chromeServerFactory.create())
      enableDomain(pub, sub)

      domainFactory.actorByName("FooTestDomain") match {
        case Some(domainActor) =>
          domainActor ! PoisonPill

          sub.expectComplete()

        case None => fail("FooTestDomain not created")
      }
    }

    "should terminate the DevTools client flow if the client disconnects" in {
      val server = chromeServerFactory.create()
      val (pub, sub) = setup(server)
      enableDomain(pub, sub)

      server.disconnect()
      sub.expectComplete()
    }

    "responds with error for an unknown domain/method" in {
      val (pub, sub) = setup(chromeServerFactory.create())

      sub.request(1)
      pub.sendNext("""{"id":"1","method":"UnknownDomain.enable"}""")
      sub.expectNext(ErrorResponse(1, unknownMethod("UnknownDomain.enable")))
    }

    "responds with error for an unknown domain/method also the second time" in {
      val (pub, sub) = setup(chromeServerFactory.create())

      sub.request(1)
      pub.sendNext("""{"id":"1","method":"UnknownDomain.enable"}""")
      sub.expectNext(ErrorResponse(1, unknownMethod("UnknownDomain.enable")))

      sub.request(1)
      pub.sendNext("""{"id":"2","method":"UnknownDomain.enable"}""")
      sub.expectNext(ErrorResponse(2, unknownMethod("UnknownDomain.enable")))
    }

    "responds with error for an unknown domain in a queued message" in {
      val (pub, sub) = setup(chromeServerFactory.create())

      sub.request(2)
      pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
      pub.sendNext("""{"id":"2","method":"UnknownDomain.enable"}""")
      sub.expectNext(EmptyResponse(1))
      sub.expectNext(ErrorResponse(2, unknownMethod("UnknownDomain.enable")))
    }

    "proceeds with the queue after an error for an unknown domain in a queued message" in {
      val (pub, sub) = setup(chromeServerFactory.create())

      sub.request(3)
      pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
      pub.sendNext("""{"id":"2","method":"UnknownDomain.enable"}""")
      pub.sendNext("""{"id":"3","method":"FooTestDomain.bar"}""")
      sub.expectNext(EmptyResponse(1))
      sub.expectNext(ErrorResponse(2, unknownMethod("UnknownDomain.enable")))
      sub.expectNext(EmptyResponse(3))
    }

    val serializeTests =
      Table(
        ("desc", "method", "resp"),
        ("normal response", "takeLock", Response(3, "lock done")),
        ("empty response", "takeLockEmpty", EmptyResponse(3)),
        ("error response", "takeLockError", ErrorResponse(3, "java.lang.RuntimeException: error"))
      )

    forAll(serializeTests) { (desc, method, expResponse) =>
      s"serializes requests to prevent domain actor races, testing $desc" in {
        val (pub, sub) = setup(chromeServerFactory.create())

        sub.request(1)
        pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
        sub.expectNext(EmptyResponse(1))
        pub.sendNext("""{"id":"2","method":"BazTestDomain.enable"}""")
        sub.expectNext(EmptyResponse(2))
        pub.sendNext(s"""{"id":"3","method":"FooTestDomain.$method"}""")
        pub.sendNext("""{"id":"4","method":"BazTestDomain.echo","params":{"msg":"testing"}}""")
        Thread.sleep(500)
        FooTestDomain.releaseLock()
        sub.expectNext(expResponse)
        sub.expectNext(Response(4, "testing"))
      }

    }
  }

  private def unknownMethod(method: String) = s"Unknown domain or method: $method"
}
