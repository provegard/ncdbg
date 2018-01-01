package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise}

class EvaluateTestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  protected def evaluateInScript(script: String, unknownEventHandler: (ScriptEvent) => Unit = _ => {})(tester: (ScriptHost, Seq[StackFrame]) => Unit): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")
    val stackframesPromise = Promise[Seq[StackFrame]]()
    val observer = Observer.from[ScriptEvent] {
      case bp: HitBreakpoint => stackframesPromise.trySuccess(bp.stackFrames)
      case x => unknownEventHandler(x)
    }
    // Newline after $script so that comments won't cause syntax error
    val wrapper =
      s"""(function () {$script
         |})();
       """.stripMargin
    observeAndRunScriptAsync(wrapper, observer) { host =>
      stackframesPromise.future.map(stackframes => {
        tester(host, stackframes)
      })
    }
  }

}
