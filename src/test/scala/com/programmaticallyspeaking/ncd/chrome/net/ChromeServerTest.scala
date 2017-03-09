package com.programmaticallyspeaking.ncd.chrome.net

import akka.actor.PoisonPill
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import akka.stream.testkit.{TestPublisher, TestSubscriber}
import akka.testkit.TestProbe
import com.programmaticallyspeaking.ncd.chrome.domains.DomainActorTesting
import com.programmaticallyspeaking.ncd.chrome.net.Protocol.{EmptyResponse, Message}
import com.programmaticallyspeaking.ncd.infra.ObjectMapping.fromJson
import com.programmaticallyspeaking.ncd.messaging.{Observable, Observer}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.reactivestreams.{Publisher, Subscriber, Subscription}
import org.scalatest.Inside

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

class ChromeServerTest extends UnitTest with DomainActorTesting with Inside {
  import AkkaTestMigration._
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

  def enableDomain(pub: TestPublisher.Probe[String], sub: TestSubscriber.Probe[Any]): Unit = {
    sub.request(1)
    pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
    sub.expectNext(EmptyResponse("1"))
  }

  "ChromeServer" - {
    "should setup a DevTools handler to respond to messages" in {
      val (pub, sub) = setup(chromeServerFactory.create())

      sub.request(1)
      pub.sendNext("""{"id":"1","method":"FooTestDomain.enable"}""")
      sub.expectNext(EmptyResponse("1"))
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
  }
}
