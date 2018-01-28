package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.{Failure, Success, Try}

class BreakpointTestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  type Tester = (ScriptHost, HitBreakpoint) => Unit

  protected def waitForBreakpoints(script: String, hostSetup: (NashornScriptHost) => Unit = (_) => {})(testers: Tester*): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")
    assert(testers.nonEmpty, "Must have at least one tester")
    val testerQueue = mutable.Queue[Tester](testers: _*)
    val donePromise = Promise[Unit]()
    val observer = Observer.from[ScriptEvent] {
      case bp: HitBreakpoint =>
        val host = getHost
        val next = testerQueue.dequeue()
        Try(next(host, bp)) match {
          case Success(_) =>
            host.resume()
            if (testerQueue.isEmpty) donePromise.success(())

          case Failure(t) =>
            donePromise.failure(t)
        }
    }
    observeAndRunScriptAsync(script, observer, hostSetup)(_ => donePromise.future)
  }

  protected def waitForBreakpoint(script: String, hostSetup: (NashornScriptHost) => Unit = (_) => {})(tester: Tester): Unit = {
    waitForBreakpoints(script, hostSetup)(tester)
  }

  protected def waitForBreakpointThenEvent(script: String, hostSetup: (NashornScriptHost) => Unit = (_) => {})
                                          (tester: (ScriptHost, HitBreakpoint) => Unit)
                                          (eventHandler: PartialFunction[ScriptEvent, Unit]): Unit = {
    assert(script.contains("debugger;"), "Script must contain a 'debugger' statement")
    val eventPromise = Promise[Unit]()
    val stackframesPromise = Promise[HitBreakpoint]()
    val observer = Observer.from[ScriptEvent] {
      case bp: HitBreakpoint => stackframesPromise.trySuccess(bp)
      case other =>
        if (eventHandler.isDefinedAt(other)) {
          eventPromise.complete(Try(eventHandler.apply(other)))
        }
    }
    observeAndRunScriptAsync(script, observer, hostSetup) { host =>
      stackframesPromise.future.flatMap(bp => {
        try tester(host, bp) finally host.resume()
        eventPromise.future
      })
    }
  }

  protected def waitForEvent(script: String, hostSetup: (NashornScriptHost) => Unit = (_) => {})(tester: PartialFunction[ScriptEvent, Unit]): Unit = {
    val eventPromise = Promise[Unit]()
    val observer = Observer.from[ScriptEvent] {
      case ev =>
        if (tester.isDefinedAt(ev)) {
          eventPromise.complete(Try(tester.apply(ev)))
        }

    }
    observeAndRunScriptAsync(script, observer, hostSetup) { host =>
      eventPromise.future
    }
  }
}
