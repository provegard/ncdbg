package com.programmaticallyspeaking.ncd.e2e

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorRef, Props}
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.{CallFrame, PausedEventParams}
import com.programmaticallyspeaking.ncd.chrome.domains.Messages
import com.programmaticallyspeaking.ncd.messaging.{Observer, SerializedSubject}
import com.programmaticallyspeaking.ncd.nashorn.NashornScriptHostTestFixture
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.exceptions.TestFailedException

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

class E2ETestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  type Tester = (Seq[CallFrame]) => Any

  // Return this from a callframe tester to prevent auto resume
  case object DontAutoResume

  private val domainEventSubject = new SerializedSubject[Messages.DomainMessage]
  private val requestor = system.actorOf(Props(new Requestor), "E2E-Requestor")
  private val currentId: AtomicInteger = new AtomicInteger(0)
  private val promises = new TrieMap[String, Promise[Any]]()

  protected def sendRequestAndWait(target: ActorRef, msg: AnyRef): Any = {
    val id = currentId.incrementAndGet()
    val promise = Promise[Any]()
    promises += id.toString -> promise
    target.tell(Messages.Request(id.toString, msg), requestor)
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

    var callFrameIdLists = Seq[Seq[String]]()
    val eventSubscription = domainEventSubject.subscribe(Observer.from[Messages.DomainMessage] {
      case Messages.Event(_, PausedEventParams(callFrames, _, _, _)) =>
        callFrameIdLists :+= callFrames.map(_.callFrameId)

        val tester = testerQueue.dequeue()
        try {
          tester(callFrames) match {
            case DontAutoResume =>
            case _ => getHost.resume()
          }

          if (testerQueue.isEmpty) {
            donePromise.trySuccess(())
          }

        } catch {
          case t: TestFailedException =>
            donePromise.tryFailure(t)
          case NonFatal(t) =>
            val ids = callFrameIdLists.map(_.mkString("[ ", ", ", " ]")).mkString("[ ", ", ", " ]")
            val errMsg = s"ERROR '${t.getMessage}' (call frame IDs: $ids), progress = \n${summarizeProgress()}"
            // Gradle shortens any stack trace too much, so suppress the stack trace of the wrapper exception
            donePromise.tryFailure(new RuntimeException(errMsg, t) {
              override def fillInStackTrace(): Throwable = this
            })
        }
    })
    donePromise.future.onComplete(_ => eventSubscription.unsubscribe())

    observeAndRunScriptAsync(script) { _ => donePromise.future }
  }

  class Requestor extends Actor {
    override def receive: Receive = {
      case msg: Messages.Accepted =>
        promises.remove(msg.id).foreach(_.trySuccess(()))

      case Messages.Response(id, data) =>
        promises.remove(id).foreach(_.trySuccess(data))

      case Messages.ErrorResponse(id, error) =>
        val msg = if (error == null) "<null>" else (if (error == "") "<unknown>" else error)
        val ex = new Exception(msg)
        promises.remove(id).foreach(_.tryFailure(ex))

      case event: Messages.DomainMessage =>
        Future { domainEventSubject.onNext(event) }
    }
  }
}
