package com.programmaticallyspeaking.ncd.testing

import akka.actor.{Actor, ActorRef, ActorSystem, Inbox, PoisonPill, Props, Terminated}
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.{Answer, OngoingStubbing}
import org.scalatest._

import scala.concurrent.duration._
import scala.language.implicitConversions
import scala.reflect.ClassTag

trait UnitTest extends FreeSpec with Matchers

trait ActorTesting extends BeforeAndAfterEach with OneInstancePerTest { self: UnitTest =>
  val receiveTimeout = 1.second

  implicit var system: ActorSystem = _

  final override protected def beforeEach(): Unit = {
    system = ActorSystem(getClass.getName.replace('.', '_'))
    beforeTest()
  }

  def beforeTest(): Unit = {}

  final override protected def afterEach(): Unit = {
    afterTest()
    system.terminate()
  }

  def afterTest(): Unit = {}

  def newActorInstance[A <: Actor : ClassTag]: ActorRef =
    system.actorOf(Props[A]())

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

