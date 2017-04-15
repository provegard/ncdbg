package com.programmaticallyspeaking.ncd.nashorn

import java.io.File
import java.nio.file.Files

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded, ScriptEvent, ScriptLocation}
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
      no(scripts.map(_.url.toString)) should endWith ("/_")
    }
  }

  "An evaluated script should have a special eval URL" in {
    val script = scriptWith("return 7 + 7;")
    whenReady(testAddScriptWithWait(script, 500.millis)) { scripts =>
      atLeast(1, scripts.map(_.url.toString)) should startWith ("eval:/")
    }
  }

  "Recompilation of eval script should be merged with the original" in {
    val script = scriptWith("var f = function (x) { return x + x; }; return f(5);")

    whenReady(testAddScriptWithWait(script, 500.millis)) { scripts =>
      val relevantScripts = scripts.filter(s => s.contents.contains("x + x"))

      relevantScripts.map(_.url.toString).distinct should have size 1
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

  "given a script with multiple breakable locations on the same line" - {
    val script =
      """function fun() { return 42; } // 2 breakable here, one in 'program' and one in 'fun'
        |fun(); // ensure compilation of 'fun'
      """.stripMargin

    lazy val f = testAddScriptWithWait(script, 500.millis)

    "only one breakable location is reported, since Nashorn/Java doesn't store column numbers" in {
      whenReady(f) { scripts =>
        scripts.find(_.contents.contains("function fun")) match {
          case Some(s) =>
            getHost.getBreakpointLocations(s.id, ScriptLocation(1, 1), Some(ScriptLocation(2, 1))) should be(Seq(ScriptLocation(1, 1)))

          case None => fail("no script")
        }
      }
    }

    "the column number is ignored when getting locations, since we cannot distinguish between column numbers anyway" in {
      whenReady(f) { scripts =>
        scripts.find(_.contents.contains("function fun")) match {
          case Some(s) =>
            getHost.getBreakpointLocations(s.id, ScriptLocation(1, 3), Some(ScriptLocation(2, 1))) should have size (1)

          case None => fail("no script")
        }
      }
    }
  }

}