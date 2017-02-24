package com.programmaticallyspeaking.ncd.nashorn

import java.io.File
import java.nio.file.Files

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded, ScriptEvent}
import com.programmaticallyspeaking.ncd.infra.DelayedFuture
import com.programmaticallyspeaking.ncd.messaging.Observer
import org.scalatest.concurrent.{Eventually, IntegrationPatience, ScalaFutures}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._

trait ScriptAddedTestFixture extends NashornScriptHostTestFixture with FairAmountOfPatience with Eventually with ScalaFutures with IntegrationPatience {
  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  def testAddScript(scriptContents: String)(handler: (Seq[Script] => Unit)): Unit = {
    var scripts = Seq.empty[Script]
    val observer = Observer.from[ScriptEvent]({
      case ScriptAdded(s) => scripts :+= s
      case _ =>
    })

    observeAndRunScriptSync(scriptContents, observer) { host =>
      eventually {
        handler(scripts)
      }
    }
  }

  def testAddScriptWithWait(scriptContents: String, waitTime: FiniteDuration): Future[Seq[Script]] = {
    var scripts = Seq.empty[Script]
    val observer = Observer.from[ScriptEvent]({
      case ScriptAdded(s) => scripts :+= s
      case _ =>
    })

    val p = Promise[Seq[Script]]()
    observeAndRunScriptSync(scriptContents, observer) { host =>
      DelayedFuture(waitTime)(p.success(scripts))
    }
    p.future
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

  "Recompilation of eval script should be merged with the original" in {

    val script =
      """(function () {
        |  var f = function (x) { return x + x; };
        |  return f(5);
        |})();
      """.stripMargin

    whenReady(testAddScriptWithWait(script, 500.millis)) { scripts =>
      val relevantScripts = scripts.filter(s => s.contents.contains("x + x"))
      relevantScripts.size should be (1)
    }
  }

  "A loaded script should result in a ScriptAdded event" in {
    val tempFile = File.createTempFile("script-to-load", ".js")
    val scriptInFile =
      """(function () {
        |  return 'hello from loaded script';
        |})();
      """.stripMargin
    Files.write(tempFile.toPath, scriptInFile.getBytes("utf-8"))
    val filePathAsUri = tempFile.toURI.toString

    val script = s"load('$filePathAsUri');"
    try {
      testAddScript(script) { scripts =>
        atLeast(1, scripts.map(_.contents)) should include("hello from loaded script")
      }
    } finally {
      Files.delete(tempFile.toPath)
    }
  }
}