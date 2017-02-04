package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise}

class BreakpointTestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global
  override val resultTimeout: FiniteDuration = 15.seconds

  protected def waitForBreakpoint(script: String)(tester: (ScriptHost, HitBreakpoint) => Unit): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")
    val stackframesPromise = Promise[HitBreakpoint]()
    val observer = new Observer[ScriptEvent] {
      override def onNext(item: ScriptEvent): Unit = item match {
        case bp: HitBreakpoint =>
          stackframesPromise.trySuccess(bp)

        case _ => // ignore
      }

      override def onError(error: Throwable): Unit = stackframesPromise.tryFailure(error)

      override def onComplete(): Unit = {}
    }
    runScriptWithObserverSync(script, observer) { host =>
      stackframesPromise.future.map(bp => {
        try tester(host, bp) finally {
          host.resume()
        }
      })
    }
  }

}
