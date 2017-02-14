package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise}
import scala.concurrent.duration._

class StepTestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  protected def stepInScript(script: String, stepTypes: Seq[StepType])(tester: (Breakpoint) => Unit): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")
    val breakpointPromise = Promise[Breakpoint]()
    val stepQueue = mutable.Queue(stepTypes: _*)
    val observer = new Observer[ScriptEvent] {
      override def onNext(item: ScriptEvent): Unit = item match {
        case bp: HitBreakpoint if stepQueue.nonEmpty =>
          val stepType = stepQueue.dequeue()
          log.debug(s"Instructing host to step with type $stepType")
          getHost.step(stepType)

        case bp: HitBreakpoint =>
          bp.stackFrames.headOption match {
            case Some(sf) => breakpointPromise.trySuccess(sf.breakpoint)
            case None => breakpointPromise.tryFailure(new Exception("No stack frame"))
          }

        case _ => // ignore
      }

      override def onError(error: Throwable): Unit = breakpointPromise.tryFailure(error)

      override def onComplete(): Unit = {}
    }
    // Add a trailing dummy line to "catch" a step-into that doesn't work, so that it steps out of the script instead.
    val wrapper =
      s"""(function() {$script})();
         |'dummy';
       """.stripMargin
    observeAndRunScriptAsync(wrapper, observer) { host =>
      breakpointPromise.future.map(breakpoint => {
        try tester(breakpoint) finally {
          host.resume()
        }
      })
    }
  }

}
