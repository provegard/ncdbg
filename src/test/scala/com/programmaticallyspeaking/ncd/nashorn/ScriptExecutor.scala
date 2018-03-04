package com.programmaticallyspeaking.ncd.nashorn

import java.io.{BufferedReader, InputStreamReader}

import com.programmaticallyspeaking.ncd.testing.StringUtils
import jdk.nashorn.api.scripting.NashornScriptEngineFactory

import scala.util.control.NonFatal

abstract class ScriptExecutorC extends App with ScriptExecutorBase {
  println("ScriptExecutor starting. Java version: " + System.getProperty("java.version"))
  val scriptEngine = new NashornScriptEngineFactory().getScriptEngine(nashornArgs: _*)
  val reader = new BufferedReader(new InputStreamReader(System.in))
  println(Signals.ready)
  waitForSignal(Signals.go)
  println("Got the go signal!")

  scriptEngine.eval(
    """this.createInstance = function (typeName) {
      |  var Type = Java.type(typeName);
      |  if (!Type) throw new Error("No such type: " + typeName);
      |  return new Type();
      |};
    """.stripMargin)

  while (true) {
    println("Awaiting script on stdin...")
    val script = StringUtils.fromBase64(readStdin())
    println("Got script: " + script)
    try {
      scriptEngine.eval(script)
      println("Script evaluation completed without errors")
    } catch {
      case NonFatal(t) =>
        t.printStackTrace(System.err)
    }
    println(Signals.scriptDone)
  }

  protected def nashornArgs: Array[String]
}

object ScriptExecutorNoJava extends ScriptExecutorC {
  override protected def nashornArgs = Array("--no-syntax-extensions", "--no-java")
}

object ScriptExecutor extends ScriptExecutorC {
  override protected def nashornArgs = Array("--no-syntax-extensions")
}