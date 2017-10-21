package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise}
import scala.concurrent.duration._

class StepTestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  protected def stepInScriptBP(script: String, stepTypes: Seq[StepType])(tester: (HitBreakpoint) => Unit): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")
    val breakpointPromise = Promise[HitBreakpoint]()
    val stepQueue = mutable.Queue(stepTypes: _*)
    val observer = new Observer[ScriptEvent] {
      override def onNext(item: ScriptEvent): Unit = item match {
        case _: HitBreakpoint if stepQueue.nonEmpty =>
          val stepType = stepQueue.dequeue()
          log.debug(s"Instructing host to step with type $stepType")
          getHost.step(stepType)

        case bp: HitBreakpoint =>
          breakpointPromise.trySuccess(bp)

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
        tester(breakpoint)
      })
    }
  }

  protected def stepInScript(script: String, stepTypes: Seq[StepType])(tester: (ScriptLocation) => Unit): Unit = {
    stepInScriptBP(script, stepTypes) { bp =>
      val location = bp.stackFrames.headOption.map(_.location).getOrElse(throw new IllegalStateException("No stack frame"))
      tester(location)
    }
  }

}
