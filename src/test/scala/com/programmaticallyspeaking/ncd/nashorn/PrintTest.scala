package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{PrintMessage, ScriptEvent}
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.concurrent.Eventually

import scala.concurrent.ExecutionContext

trait PrintTestFixture extends NashornScriptHostTestFixture with Eventually with FairAmountOfPatience {
  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  def runScriptAndCollectEvents(script: String)(handler: Seq[PrintMessage] => Unit) = {
    var events = Seq.empty[PrintMessage]
    val obs = Observer.from[ScriptEvent] {
      case ev: PrintMessage => events :+= ev
      case _ =>
    }

    observeAndRunScriptSync(script, obs) { _ =>
      eventually {
        //handler(events)
        assert(events.nonEmpty)
      }
      handler(events)
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
