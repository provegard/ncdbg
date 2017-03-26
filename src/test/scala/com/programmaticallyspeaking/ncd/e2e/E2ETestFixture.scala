package com.programmaticallyspeaking.ncd.e2e

import akka.actor.{Actor, ActorRef, Props}
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.{CallFrame, PausedEventParams}
import com.programmaticallyspeaking.ncd.chrome.domains.Messages
import com.programmaticallyspeaking.ncd.messaging.{Observer, SerializedSubject, Subject}
import com.programmaticallyspeaking.ncd.nashorn.NashornScriptHostTestFixture
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

class E2ETestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  type Tester = (Seq[CallFrame]) => Unit

  private val messageSubject = new SerializedSubject[Messages.DomainMessage]
  private val requestor = system.actorOf(Props(new Requestor(messageSubject)), "E2E-Requestor")
  private var currentId: Int = 0
  private val promises = new TrieMap[String, Promise[Any]]()

  protected def sendRequestAndWait(target: ActorRef, msg: AnyRef): Any = {
    currentId += 1
    val promise = Promise[Any]()
    promises += currentId.toString -> promise
    target.tell(Messages.Request(currentId.toString, msg), requestor)
    Await.result(promise.future, resultTimeout)
  }

  override def sendAndReceive(actorRef: ActorRef, msg: AnyRef): Any =
    throw new IllegalStateException("Use sendRequestAndWait instead")

  override def sendAndReceiveMatching[T](actorRef: ActorRef, msg: AnyRef)(pf: PartialFunction[Any, T]): T =
    throw new IllegalStateException("Use sendRequestAndWait instead")

  protected def runScript(script: String)(testers: Tester*): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")

    val donePromise = Promise[Unit]()
    val testerQueue = mutable.Queue(testers: _*)

    val subscription = messageSubject.subscribe(new Observer[Messages.DomainMessage] {
      override def onError(error: Throwable): Unit = donePromise.tryFailure(error)

      override def onComplete(): Unit = donePromise.trySuccess(())

      override def onNext(item: Messages.DomainMessage): Unit = item match {
        case Messages.Event(_, PausedEventParams(callFrames, _, _)) =>

          val tester = testerQueue.dequeue()
          try {
            tester(callFrames)
            getHost.resume()

            if (testerQueue.isEmpty) {
              donePromise.trySuccess(())
            }

          } catch {
            case NonFatal(t) =>
              donePromise.tryFailure(t)
          }

        case _ => // ignore
      }
    })
    donePromise.future.onComplete(_ => subscription.unsubscribe())

    observeAndRunScriptAsync(script) { _ => donePromise.future }
  }

  class Requestor(subject: Subject[Messages.DomainMessage]) extends Actor {
    override def receive: Receive = {
      case msg: Messages.Accepted =>
        promises.remove(msg.id).foreach(_.trySuccess(()))

      case Messages.Response(id, data) =>
        promises.remove(id).foreach(_.trySuccess(data))

      case Messages.ErrorResponse(id, error) =>
        val ex = new Exception(error)
        promises.remove(id).foreach(_.tryFailure(ex))
        Future { subject.onError(ex) }

      case msg: Messages.DomainMessage =>
        Future { subject.onNext(msg) }
    }
  }
}
