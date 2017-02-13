package com.programmaticallyspeaking.ncd.nashorn
import java.io.{BufferedReader, InputStreamReader}
import java.util.concurrent.atomic.AtomicInteger
import javax.script.Compilable

import com.programmaticallyspeaking.ncd.host.{HitBreakpoint, Script, ScriptAdded, ScriptEvent}
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.{FreeActorTesting, UnitTest}
import jdk.nashorn.api.scripting.NashornScriptEngineFactory
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.slf4s.Logging

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

trait MultiThreadingTestFixture extends UnitTest with Logging with FreeActorTesting with VirtualMachineLauncher with ScalaFutures with IntegrationPatience {
  override val scriptExecutor: ScriptExecutorBase = MultiThreadedScriptExecutor
  override implicit val executionContext: ExecutionContext = ExecutionContext.global
  override val resultTimeout: FiniteDuration = 5.seconds

  def ready = vmRunningPromise.future
}

class MultiThreadingTest extends MultiThreadingTestFixture {

  "Breakpoint requests from other threads should be ignore in a paused state" in {
    val scriptAddedPromise = Promise[Script]()
    val hitBreakpointPromise = Promise[String]()
    val breakpointCounter = new AtomicInteger()
    eventSubject.subscribe(new Observer[ScriptEvent] {

      override def onNext(item: ScriptEvent): Unit = item match {
        case ScriptAdded(script) => scriptAddedPromise.success(script)
        case hb: HitBreakpoint =>
          breakpointCounter.incrementAndGet()
          hitBreakpointPromise.trySuccess("")
        case _ =>
      }

      override def onError(error: Throwable): Unit = {}

      override def onComplete(): Unit = {}
    })

    whenReady(ready) { host =>
      whenReady(scriptAddedPromise.future) { script =>
        val lineNumber = host.getBreakpointLineNumbers(script.id, 1, None).headOption match {
          case Some(l) => l
          case None => fail(s"No line numbers for script ${script.id}")
        }
        host.setBreakpoint(script.uri, lineNumber)
        whenReady(hitBreakpointPromise.future) { _ =>
          // Ugly, but wait for a while to see if the counter increases over 1 (which it shouldn't).
          Thread.sleep(200)
          breakpointCounter.get() should be (1)
        }
      }
    }
  }
}

object MultiThreadedScriptExecutor extends App with ScriptExecutorBase {
  println("MultiThreadedScriptExecutor starting. Java version: " + System.getProperty("java.version"))
  val scriptEngine = new NashornScriptEngineFactory().getScriptEngine
  val reader = new BufferedReader(new InputStreamReader(System.in))
  waitForSignal("go")

  val compiledScript = scriptEngine.asInstanceOf[Compilable].compile(
    """(function () {
      |  return 5 + 5;
      |})();
    """.stripMargin)

  implicit val ec = ExecutionContext.global

  val futures = (1 to 5).map { _ =>
    Future {
      while (true) {
        compiledScript.eval()
      }
    }
  }

  Await.result(Future.sequence(futures), 30.seconds)
}