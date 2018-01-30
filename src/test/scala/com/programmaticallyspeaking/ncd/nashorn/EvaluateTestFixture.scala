package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.{Failure, Success, Try}

class EvaluateTestFixture extends UnitTest with NashornScriptHostTestFixture with ScalaFutures with IntegrationPatience {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  type Tester = (ScriptHost, Seq[StackFrame]) => Unit

  protected def evaluateInScript(script: String, unknownEventHandler: (ScriptEvent) => Unit = _ => {})(testers: Tester*): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")
    assert(testers.nonEmpty, "Must have at least one tester")
    val testerQueue = mutable.Queue[Tester](testers: _*)
    val donePromise = Promise[Unit]()
    val observer = Observer.from[ScriptEvent] {
      case bp: HitBreakpoint =>
        val host = getHost
        val next = testerQueue.dequeue()
        Try(next(host, bp.stackFrames)) match {
          case Success(_) =>
            host.resume()
            if (testerQueue.isEmpty) donePromise.success(())

          case Failure(t) =>
            donePromise.failure(t)
        }
      case x => unknownEventHandler(x)
    }
    observeAndRunScriptAsync(script, observer)(_ => donePromise.future)
  }
}
