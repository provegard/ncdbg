package com.programmaticallyspeaking.ncd.nashorn

import java.io.{BufferedReader, InputStreamReader}

import com.programmaticallyspeaking.ncd.testing.StringUtils
import jdk.nashorn.api.scripting.NashornScriptEngineFactory

import scala.concurrent.duration._

/**
  * When the debugger connects to [[RunningApp]], it will/may see the Recompilation script first, and the single
  * test in this class verifies that the scripts are merged correctly also when they are observed in that order.
  */
class ScriptIdentificationInRunningAppTest extends ScriptAddedTestFixture {

  "Recompilation of eval script should be merged with the original - regardless of observed order" in {
    val script = "'dummy';"

    whenReady(testAddScriptWithWait(script, 500.millis)) { _ =>
      getHost.scripts.filterNot(_.contents.contains("dummy")).map(_.url).distinct should have size 1
    }
  }

  override val scriptExecutor = RunningApp
}

object RunningApp extends App with ScriptExecutorBase {
  println("RunningApp starting. Java version: " + System.getProperty("java.version"))
  val scriptEngine = new NashornScriptEngineFactory().getScriptEngine("--no-syntax-extensions")
  val reader = new BufferedReader(new InputStreamReader(System.in))

  val script1 =
    """function fun() {
      |  return '42';
      |}
      |var i = 0;
      |while (i++ < 10) {
      |  fun();
      |}
    """.stripMargin
  scriptEngine.eval(script1)

  // Stuff required by the test infra
  println(Signals.ready)
  waitForSignal(Signals.go)
  val script = StringUtils.fromBase64(readStdin())
  scriptEngine.eval(script)
  println(Signals.scriptDone)
}