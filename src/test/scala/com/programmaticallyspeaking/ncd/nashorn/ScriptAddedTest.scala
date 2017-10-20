package com.programmaticallyspeaking.ncd.nashorn

import java.io.File
import java.nio.file.Files

import com.programmaticallyspeaking.ncd.host._
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

  "A script should be identifiable with a regexp-based identity" in {
    testAddScript(scriptWith("return 9 + 9;")) { scripts =>
      val script = scripts.head
      val regexped = script.url.toString.replace("eval:", ".*:")
      val id = ScriptIdentity.fromURLRegex(regexped)
      getHost.findScript(id).map(_.id) should be (Some(script.id))
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

  "a loaded script with relative path results in a ScriptAdded event" in {
    val script = "load({ script: 'this.foobar=42;', name: 'path/to/myscript.js' });"
    testAddScript(script) { scripts =>
      atLeast(1, scripts.map(_.contents)) should be ("this.foobar=42;")
    }
  }

  "a loaded script with relative Windows path results in a ScriptAdded event" in {
    val script = "load({ script: 'this.barbar=42;', name: 'path\\to\\yourscript.js' });"
    testAddScript(script) { scripts =>
      atLeast(1, scripts.map(_.contents)) should be ("this.barbar=42;")
    }
  }

  "given a script with multiple breakable locations on the same line" - {
    val script =
      """function fun() { return 42; } // 2 breakable here, one in 'program' and one in 'fun'
        |fun(); // ensure compilation of 'fun'
      """.stripMargin

    lazy val f = testAddScriptWithWait(script, 500.millis)

    "multiple breakable locations are no longer reported, since it's too fragile" in {
      whenReady(f) { scripts =>
        scripts.find(_.contents.contains("function fun")) match {
          case Some(s) =>
            getHost.getBreakpointLocations(ScriptIdentity.fromId(s.id), ScriptLocation(1, Some(1)), Some(ScriptLocation(2, Some(1)))) should
              be(Seq(ScriptLocation(1, Some(1))))

          case None => fail("no script")
        }
      }
    }

    "the column number is ignored when getting locations, since a bad source map translation otherwise may make it hard to set an accurate breakpoint" in {
      whenReady(f) { scripts =>
        scripts.find(_.contents.contains("function fun")) match {
          case Some(s) =>
            getHost.getBreakpointLocations(ScriptIdentity.fromId(s.id), ScriptLocation(1, Some(3)), Some(ScriptLocation(2, Some(1)))) should have size (1)

          case None => fail("no script")
        }
      }
    }
  }

  "with multiple breakable locations on a line with an IIFE" - {
    val script =
      """function funnier(number) {
        |  if (number % 2 === 0) return (function () { return 41; })();
        |  return 42;
        |}
        |funnier(0);
      """.stripMargin
    lazy val f = testAddScriptWithWait(script, 1000.millis)

    // Note: Test kept but ignored (rather than changed) because it's redundant now but can be useful if we
    // re-enable column guessing.
    "multiple breakable locations are reported" ignore {
      whenReady(f) { scripts =>
        scripts.find(_.contents.contains("funnier")) match {
          case Some(s) =>
            getHost.getBreakpointLocations(ScriptIdentity.fromId(s.id), ScriptLocation(2, Some(1)), Some(ScriptLocation(3, Some(1)))) should be(Seq(
              ScriptLocation(2, Some(3)), ScriptLocation(2, Some(47))))

          case None => fail("no script")
        }
      }
    }
  }

  "with multiple line locations in the same function where an if statement is last" - {
    val script =
      """function value() { return 0; }
        |function fun() {
        |  if ((function () { return value(); })() > 4) {
        |    value(5);
        |  }
        |}
        |fun();
      """.stripMargin
    lazy val f = testAddScriptWithWait(script, 1000.millis)

    // Note: Test kept but ignored (rather than changed) because it's redundant now but can be useful if we
    // re-enable column guessing.
    "multiple breakable locations for the different functions are reported" ignore {
      whenReady(f) { scripts =>
        scripts.find(_.contents.contains("value()")) match {
          case Some(s) =>
            getHost.getBreakpointLocations(ScriptIdentity.fromId(s.id), ScriptLocation(3, Some(1)), Some(ScriptLocation(4, Some(1)))) should be(Seq(
              ScriptLocation(3, Some(3)), ScriptLocation(3, Some(22))))

          case None => fail("no script")
        }
      }
    }
  }
}