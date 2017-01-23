package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise}

class EvaluateTestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global
  override val resultTimeout: FiniteDuration = 15.seconds

  protected def evaluateInScript(script: String)(tester: (ScriptHost, Seq[StackFrame]) => Unit): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")
    val stackframesPromise = Promise[Seq[StackFrame]]()
    val observer = new Observer[ScriptEvent] {
      override def onNext(item: ScriptEvent): Unit = item match {
        case bp: HitBreakpoint =>
          stackframesPromise.trySuccess(bp.stackFrames)

        case _ => // ignore
      }

      override def onError(error: Throwable): Unit = stackframesPromise.tryFailure(error)

      override def onComplete(): Unit = {}
    }
    // Newline after $script so that comments won't cause syntax error
    val wrapper =
      s"""(function () {$script
         |})();
       """.stripMargin
    runScriptWithObserverSync(wrapper, observer) { host =>
      stackframesPromise.future.map(stackframes => {
        try tester(host, stackframes) finally {
          host.resume()
        }
      })
    }
  }

}
