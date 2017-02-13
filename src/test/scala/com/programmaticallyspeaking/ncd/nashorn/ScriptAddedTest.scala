package com.programmaticallyspeaking.ncd.nashorn

import java.io.{BufferedReader, InputStreamReader}
import javax.script.Compilable

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded, ScriptEvent}
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.{FreeActorTesting, UnitTest}
import jdk.nashorn.api.scripting.NashornScriptEngineFactory
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.slf4s.Logging

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise}

trait ScriptAddedTestFixture extends UnitTest with Logging with FreeActorTesting with VirtualMachineLauncher with ScalaFutures with IntegrationPatience {
  override val scriptExecutor: ScriptExecutorBase = ScriptAddingScriptExecutor
  override implicit val executionContext: ExecutionContext = ExecutionContext.global
  override val resultTimeout: FiniteDuration = 5.seconds

  def ready = vmRunningPromise.future
}

class ScriptAddedTest extends ScriptAddedTestFixture {

  "An added script should result in a ScriptAdded event" in {
    val scriptAddedPromise = Promise[Script]()
    eventSubject.subscribe(new Observer[ScriptEvent] {

      override def onNext(item: ScriptEvent): Unit = item match {
        case ScriptAdded(script) => scriptAddedPromise.success(script)
        case _ =>
      }

      override def onError(error: Throwable): Unit = {}

      override def onComplete(): Unit = {}
    })

    whenReady(ready) { _ =>
      sendToVm("add")
      whenReady(scriptAddedPromise.future) { script =>
        script.contents should include ("5 + 5")
      }
    }
  }
}

object ScriptAddingScriptExecutor extends App with ScriptExecutorBase {
  println("MultiThreadedScriptExecutor starting. Java version: " + System.getProperty("java.version"))
  val scriptEngine = new NashornScriptEngineFactory().getScriptEngine
  val reader = new BufferedReader(new InputStreamReader(System.in))

  waitForSignal("go")
  waitForSignal("add")

  val compiledScript = scriptEngine.asInstanceOf[Compilable].compile(
    """(function () {
      |  return 5 + 5;
      |})();
    """.stripMargin)

  println("Waiting...")
  readStdin()

  private def readStdin(): String = reader.readLine()
  private def waitForSignal(expected: String): Unit = {
    println(s"Awaiting '$expected' signal")
    val signal = readStdin()
    if (signal != expected) {
      println(s"Didn't get '$expected' signal, got: " + signal)
      System.exit(1)
    }
  }
}