package com.programmaticallyspeaking.ncd.nashorn

import java.io.File
import java.nio.file.Files

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded, ScriptEvent}
import com.programmaticallyspeaking.ncd.messaging.Observer
import org.scalatest.concurrent.{Eventually, ScalaFutures}

import scala.concurrent.ExecutionContext

trait ScriptAddedTestFixture extends NashornScriptHostTestFixture with ScalaFutures with FairAmountOfPatience with Eventually {
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