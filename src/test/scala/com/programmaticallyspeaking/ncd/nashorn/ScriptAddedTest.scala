package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded, ScriptEvent}
import com.programmaticallyspeaking.ncd.messaging.Observer
import org.scalatest.concurrent.{Eventually, IntegrationPatience, ScalaFutures}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

trait ScriptAddedTestFixture extends NashornScriptHostTestFixture with ScalaFutures with IntegrationPatience with Eventually {
  override implicit val executionContext: ExecutionContext = ExecutionContext.global
  override val resultTimeout: FiniteDuration = 10.seconds

  def testAddScript(scriptContents: String)(handler: (Seq[Script] => Unit)): Unit = {
    var scripts = Seq.empty[Script]
    val observer = Observer.from[ScriptEvent]({
      case ScriptAdded(s) => scripts :+= s
      case _ =>
    })

    eventSubject.subscribe(observer)

    val script =
      """(function () {
        |  return 5 + 5;
        |})();
      """.stripMargin

    val f = vmRunningPromise.future.map { host =>
      sendToVm(script, encodeBase64 = true)

      eventually {
        handler(scripts)
      }
    }
    Await.result(f, resultTimeout)
  }
}

class ScriptAddedTest extends ScriptAddedTestFixture {

  "An added script should result in a ScriptAdded event" in {

    val script =
      """(function () {
        |  return 5 + 5;
        |})();
      """.stripMargin

    testAddScript(script) { scripts =>
      atLeast(1, scripts.map(_.contents)) should include ("5 + 5")
    }
  }
}