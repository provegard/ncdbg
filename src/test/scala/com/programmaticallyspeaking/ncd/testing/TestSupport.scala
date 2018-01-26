package com.programmaticallyspeaking.ncd.testing

import akka.actor.{Actor, ActorRef, ActorSystem, Inbox, PoisonPill, Props, Terminated}
import com.programmaticallyspeaking.ncd.ioc.Container
import com.typesafe.config.ConfigFactory
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.{Answer, OngoingStubbing}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait UnitTest extends FreeSpec with Matchers
trait IsolatedUnitTest extends path.FreeSpec with Matchers

trait AsyncUnitTest extends UnitTest with ScalaFutures {
  implicit val executionContext: ExecutionContext = ExecutionContext.global
}

object SelectiveActorLogging {
  def config = {
    val suppress = System.getProperty("SelectiveActorLogging.suppress") == "true"
    val configStr = if (suppress) {
      """
        |akka.loggers = []
        |akka.stdout-loglevel = "OFF"
        |akka.loglevel = "OFF"
        |akka.log-dead-letters = off
      """.stripMargin
    } else ""
    ConfigFactory.parseString(configStr)
  }
}

trait ActorOperations {
  val receiveTimeout: FiniteDuration

  implicit val system: ActorSystem

  def sendAndReceive(actorRef: ActorRef, msg: AnyRef): Any = {
    val inbox = Inbox.create(system)
    inbox.send(actorRef, msg)
    inbox.receive(receiveTimeout)
  }

  def sendAndReceiveMatching[T](actorRef: ActorRef, msg: AnyRef)(pf: PartialFunction[Any, T]): T = {
    val inbox = Inbox.create(system)
    inbox.watch(actorRef) // needed for terminateAndWait
    inbox.send(actorRef, msg)
    while (true) {
      val received = inbox.receive(receiveTimeout)
      if (pf.isDefinedAt(received)) return pf(received)
    }
    ??? // shouldn't get here
  }

  def terminateAndWait(actor: ActorRef): Unit = {
    sendAndReceiveMatching(actor, PoisonPill) {
      case Terminated(a) if a == actor => // ok
    }
  }

  def newActorInstance[A <: Actor : ClassTag](implicit container: Container = Container.empty): ActorRef = {
    val clazz = implicitly[ClassTag[A]].runtimeClass
    def creator(clazz: Class[_], args: Seq[Any]) = Props(clazz, args: _*)
    system.actorOf(container.newInstance(clazz, creator))
  }

}

trait ActorTesting extends BeforeAndAfterEach with OneInstancePerTest with ActorOperations { self: UnitTest =>
  val receiveTimeout = 1.second

  implicit val system: ActorSystem = createActorSystem

  def createActorSystem = ActorSystem(getClass.getName.replace('.', '_'), SelectiveActorLogging.config)

  final override protected def beforeEach(): Unit = {
    beforeTest()
  }

  def beforeTest(): Unit = {}

  final override protected def afterEach(): Unit = {
    afterTest()
    system.terminate()
  }

  def afterTest(): Unit = {}
}

/**
  * Unlike [[ActorTesting]], which mixes in [[OneInstancePerTest]]
  */
trait SharedInstanceActorTesting extends BeforeAndAfterEach with BeforeAndAfterAll with ActorOperations { self: UnitTest =>
  val receiveTimeout = 1.second

  implicit val system: ActorSystem = createActorSystem

  def createActorSystem = ActorSystem(getClass.getName.replace('.', '_'), SelectiveActorLogging.config)

  final override protected def beforeEach(): Unit = {
    beforeTest()
  }

  def beforeTest(): Unit = {}
  def afterTest(): Unit = {}
  def beforeAllTests(): Unit = {}
  def afterAllTests(): Unit = {}

  final override protected def afterEach(): Unit = {
    afterTest()
  }

  final override protected def beforeAll(): Unit = beforeAllTests()

  final override protected def afterAll(): Unit =  try afterAllTests() finally system.terminate()
}

// TODO: Why can't we have a trait Mocking that does `import Mocking._` where Mocking is this object??
object MockingUtils {
  implicit def ongoingStubbing2ExtStubbing[T](os: OngoingStubbing[T]): ExtStubbing[T] = new ExtStubbing(os)

  class ExtStubbing[T](os: OngoingStubbing[T]) {
    // TODO: Can this be named thenAnswer? It doesn't seem to work since thenAnswer also is a regular function in OngoingStubbing.
    def thenAnswerWith(pf: PartialFunction[List[AnyRef], T]): OngoingStubbing[T] = {
      os.thenAnswer(new Answer[T] {
        override def answer(invocation: InvocationOnMock): T = {
          val args = invocation.getArguments.toList
          pf.applyOrElse(args, (x: List[AnyRef]) => throw new IllegalArgumentException("Failed to provide an answer for: " + args.mkString(", ")))
        }
      })
      os
    }
  }
}

