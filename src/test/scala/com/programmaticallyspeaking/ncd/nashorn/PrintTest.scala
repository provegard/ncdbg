package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{HitBreakpoint, PrintMessage, ScriptEvent, StackFrame}
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.concurrent.Eventually

import scala.concurrent.{ExecutionContext, Promise}

trait PrintTestFixture extends NashornScriptHostTestFixture with Eventually with FairAmountOfPatience {
  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  def runScriptAndCollectEvents(script: String)(handler: Seq[PrintMessage] => Unit) = {
    var events = Seq.empty[PrintMessage]
    val obs = Observer.from[ScriptEvent] {
      case ev: PrintMessage => events :+= ev
    }

    observeAndRunScriptSync(script, obs) { host =>
      eventually {
        assert(events.nonEmpty)
      }
      handler(events)
    }
  }

  def runScriptAndCollectEventsWhilePaused(code: String)(handler: Seq[PrintMessage] => Unit) = {
    var events = Seq.empty[PrintMessage]
    val stackframesPromise = Promise[Seq[StackFrame]]()
    val obs = Observer.from[ScriptEvent] {
      case ev: PrintMessage => events :+= ev
      case bp: HitBreakpoint => stackframesPromise.success(bp.stackFrames)
    }

    observeAndRunScriptAsync("debugger;", obs) { host =>
      stackframesPromise.future.map { sf =>

        host.evaluateOnStackFrame(sf.head.id, code)

        handler(events)
      }
    }
  }
}

class PrintTest extends UnitTest with PrintTestFixture {

  "Capture of Nashorn's print extension" - {

    "emits a PrintMessage event" in {
      expectMessage("print('hello world');", "hello world")
    }

    "puts space inbetween arguments" in {
      expectMessage("print('hello', 'world');", "hello world")
    }

    "uses JS stringification" in {
      expectMessage("print({});", "[object Object]")
    }

    "handles null" in {
      expectMessage("print(null, 'foo');", "null foo")
    }

    "emits a PrintMessage even if the no-newline version is used" in {
      useNashornArguments(Seq("print-no-newline"))
      expectMessage("print('hello world');", "hello world")
    }

    "is ignored when the debugger is paused to avoid deadlock" in {
      runScriptAndCollectEventsWhilePaused("print('ignored');") { events =>
        events should be ('empty)
      }
    }
  }

  private def expectMessage(script: String, message: String): Unit = {
    runScriptAndCollectEvents(script) { events =>
      expectMessage(events, message)
    }
  }

  private def expectMessage(events: Seq[PrintMessage], message: String): Unit = {
    val found = events.find(_.message == message)
    found should be ('defined)
  }

}
