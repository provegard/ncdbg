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
  def scriptWith(src: String) =
    s"""(function () {
       |  $src
       |})();
     """.stripMargin

  "An added script should result in a ScriptAdded event" in {
    testAddScript(scriptWith("return 5 + 5;")) { scripts =>
      atLeast(1, scripts.map(_.contents)) should include ("5 + 5")
    }
  }

  "An evaluated script should not have an URL that ends with a single underscore" in {
    val script = scriptWith("return 6 + 6;")
    whenReady(testAddScriptWithWait(script, 500.millis)) { scripts =>
      no(scripts.map(_.uri)) should endWith ("/_")
    }
  }

  "Recompilation of eval script should be merged with the original" in {
    val script = scriptWith("var f = function (x) { return x + x; }; return f(5);")

    whenReady(testAddScriptWithWait(script, 500.millis)) { scripts =>
      val relevantScripts = scripts.filter(s => s.contents.contains("x + x"))
      relevantScripts.size should be (1)
    }
  }

  "A loaded script should result in a ScriptAdded event" in {
    val tempFile = File.createTempFile("script-to-load", ".js")
    val scriptInFile = scriptWith("return 'hello from loaded script';")
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