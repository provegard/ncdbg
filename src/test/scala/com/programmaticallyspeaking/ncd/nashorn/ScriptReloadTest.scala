package com.programmaticallyspeaking.ncd.nashorn

import java.io.File
import java.nio.file.Files

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.{Observable, Observer, Subject}
import org.scalatest.concurrent.{Eventually, ScalaFutures}

import scala.concurrent.{ExecutionContext, Future, Promise}

trait ScriptReloadTestFixture extends NashornScriptHostTestFixture with ScalaFutures with FairAmountOfPatience with Eventually {
  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  private val scriptFile = File.createTempFile("script", ".js")
  scriptFile.deleteOnExit()
  private val jsPath = scriptFile.getAbsolutePath.replace('\\', '/').replace("'", "\\'")

  private var loaderId = 0
  private def loader = {
    // make the loader unique each time to prevent the test infra from resetting the debugger
    val id = loaderId
    loaderId += 1
    s"""
       |load('$jsPath'); // $id
       |//debugger; <-- dummy, required by test infra
       """.stripMargin
  }
  private def writeScript(script: String): Unit = Files.write(scriptFile.toPath, script.getBytes("utf-8"))

  def loadScript(source: String): Future[Script] = {
    val p = Promise[Script]()
    writeScript(source)
    val observer = Observer.from[ScriptEvent] {
      case sa: ScriptAdded if sa.script.contents == source =>
        p.success(sa.script)
    }
    observeAndRunScriptAsync(loader, observer)(_ => Future.successful(()))
    p.future
  }

  def executeScriptThatLoads(source: String)(cb: HitBreakpoint => Unit) = {
    var error: Option[String] = None
    val observer = Observer.from[ScriptEvent] {
      case hb: HitBreakpoint =>
        cb(hb)
        getHost.resume()
      case PrintMessage(msg) =>
        println("ScriptReloadTest: " + msg)
      case UncaughtError(errorValue) =>
        error = Some("ERROR: " + errorValue.data.message)
    }
    observeAndRunScriptAsync(source, observer)(_ => Future.successful(()))
    error.foreach(e => fail(e))
  }
}

class ScriptReloadTest extends ScriptReloadTestFixture {

  val script1 = "this.x = 42;"
  val script2 = "this.y = 43;"

  "when a script path is loaded" - {
    val fScript1 = loadScript(script1)

    "and then reloaded with new contents" - {
      val fScript2 = fScript1.flatMap(_ => loadScript(script2))

      "it gets a new ID" in {
        whenReady(fScript1) { script =>
          whenReady(fScript2) { scriptAgain =>
            scriptAgain.id should not be (script.id)
          }
        }
      }

      "it has new contents" in {
        whenReady(fScript2) { scriptAgain =>
          scriptAgain.contents should include("this.y")
        }
      }

      "it has the same URL as the original" in {
        whenReady(fScript1) { script =>
          whenReady(fScript2) { scriptAgain =>
            scriptAgain.url.toString should be (script.url.toString)
          }
        }
      }

      "it can be fetched from the host" in {
        whenReady(fScript2) { scriptAgain =>
          scriptById(scriptAgain.id).contents should include("this.y")
        }
      }

      "the old script can no longer be fetched from the host" in {
        whenReady(fScript1) { script =>
          whenReady(fScript2) { _ =>
            assertThrows[IllegalArgumentException](scriptById(script.id))
          }
        }
      }
    }
  }

  "it should ignore a 'debugger' breakpoint in a replaced script" in {
    val script =
      """
        |var fun1 = load({
        |  script: "(function script1() { debugger; })",
        |  name: "myscript.js"
        |});
        |
        |var fun2 = load({
        |  script: "(function script2() { debugger; })",
        |  name: "myscript.js"
        |});
        |
        |fun1();
        |fun2();
      """.stripMargin

    var functionNames = Seq.empty[String]
    executeScriptThatLoads(script) { hb =>
      val name = hb.stackFrames.head.functionDetails.name
      functionNames :+= name
    }
    eventually {
      functionNames should be (Seq("script2"))
    }
  }

  "it should ignore an ID-based breakpoint in a replaced script" in {
    val script =
      """
        |// because embedding newlines in the script bodies doesn't work
        |function replaceNL(s) {
        |  return s.replaceAll("_NL_", String.fromCharCode(10));
        |}
        |function main() {
        |  var fun1 = load({
        |    script: replaceNL("(function script1() {_NL_Math.random();_NL_debugger;_NL_})"),
        |    name: "myscriptz.js"
        |  });
        |  fun1(); // call fun1 and set the breakpoint when we're paused at 'debugger'
        |
        |  load({
        |    script: "(function script2() {})",
        |    name: "myscriptz.js"
        |  });
        |
        |  // call fun1 again - shouldn't stop at breakpoint _or_ 'debugger'
        |  fun1();
        |  debugger; // should stop here
        |}
        |main();
      """.stripMargin

    var functionNames = Seq.empty[String]
    executeScriptThatLoads(script) { hb =>
      val functionName = hb.stackFrames.head.functionDetails.name
      if (functionName == "script1") {
        println("Here")
        val scriptId = hb.stackFrames.head.scriptId
        // Set a breakpoint on the line before 'debugger'
        var scriptLoc = hb.stackFrames.head.location
        scriptLoc = scriptLoc.copy(lineNumber1Based = scriptLoc.lineNumber1Based - 1)
        println(scriptLoc)
        getHost.setBreakpoint(IdBasedScriptIdentity(scriptId), scriptLoc, BreakpointOptions(None, false))
      }
      functionNames :+= functionName
    }
    eventually {
      functionNames should be (Seq("script1", "main"))
    }
  }

  private def scriptById(id: String): Script = {
    getHost.findScript(ScriptIdentity.fromId(id)).getOrElse(throw new IllegalArgumentException("No script with ID " + id))
  }
}
