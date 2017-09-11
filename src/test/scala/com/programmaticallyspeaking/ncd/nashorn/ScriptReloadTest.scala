package com.programmaticallyspeaking.ncd.nashorn

import java.io.File
import java.nio.file.Files

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded, ScriptEvent, ScriptIdentity}
import com.programmaticallyspeaking.ncd.messaging.Observer
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{ExecutionContext, Future, Promise}

trait ScriptReloadTestFixture extends NashornScriptHostTestFixture with ScalaFutures with FairAmountOfPatience {
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

      "it has a modified URL to not collide with the original" in {
        whenReady(fScript1) { script =>
          whenReady(fScript2) { scriptAgain =>
            scriptAgain.url.toString should not be (script.url.toString)
          }
        }
      }

      "it can be fetched from the host" in {
        whenReady(fScript2) { scriptAgain =>
          scriptById(scriptAgain.id).contents should include("this.y")
        }
      }

      "the old script can still be fetched from the host" in {
        whenReady(fScript1) { script =>
          whenReady(fScript2) { _ =>
            scriptById(script.id).contents should include("this.x")
          }
        }
      }
    }
  }

  private def scriptById(id: String): Script = {
    getHost.findScript(ScriptIdentity.fromId(id)).getOrElse(throw new IllegalArgumentException("No script with ID " + id))
  }
}
