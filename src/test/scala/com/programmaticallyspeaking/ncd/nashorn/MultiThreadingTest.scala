package com.programmaticallyspeaking.ncd.nashorn
import java.io.{BufferedReader, InputStreamReader}
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.{SharedInstanceActorTesting, UnitTest}
import jdk.nashorn.api.scripting.NashornScriptEngineFactory
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.exceptions.TestFailedException
import org.slf4s.Logging

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

trait MultiThreadingTestFixture extends UnitTest with Logging with SharedInstanceActorTesting with VirtualMachineLauncher with ScalaFutures with FairAmountOfPatience with Eventually {
  override val scriptExecutor: ScriptExecutorBase = MultiThreadedScriptExecutor
  override implicit val executionContext: ExecutionContext = ExecutionContext.global
}

class MultiThreadingTest extends MultiThreadingTestFixture {
  def location(ln: Int) = ScriptLocation(ln, None)

  "Breakpoint requests from other threads should be ignore in a paused state" in {
    val scriptAddedPromise = Promise[Script]()
    val hitBreakpointPromise = Promise[String]()
    val breakpointCounter = new AtomicInteger()
    val host = getHost
    observeScriptEvents(new Observer[ScriptEvent] {

      override def onNext(item: ScriptEvent): Unit = item match {
        case ScriptAdded(script) =>
          scriptAddedPromise.success(script)
        case hb: HitBreakpoint =>
          breakpointCounter.incrementAndGet()
          hitBreakpointPromise.trySuccess("")
        case _ =>
      }

      override def onError(error: Throwable): Unit = {}

      override def onComplete(): Unit = {}
    })

    whenReady(scriptAddedPromise.future) { script =>
      val scriptLocation = eventually {
        host.getBreakpointLocations(ScriptIdentity.fromId(script.id), location(1), None).headOption.getOrElse(fail(s"No line numbers for script ${script.id}"))
      }
      host.setBreakpoint(ScriptIdentity.fromURL(script.url), scriptLocation, None)

      try {
        whenReady(hitBreakpointPromise.future) { _ =>
          // Ugly, but wait for a while to see if the counter increases over 1 (which it shouldn't).
          Thread.sleep(200)
          breakpointCounter.get() should be(1)
        }
      } catch {
        case t: TestFailedException if t.getMessage().contains("timeout") =>
          val progress = summarizeProgress()
          throw new TimeoutException("Timed out: " + progress)
      }
    }
  }
}

object MultiThreadedScriptExecutor extends App with ScriptExecutorBase {
  println("MultiThreadedScriptExecutor starting. Java version: " + System.getProperty("java.version"))
  val scriptEngine = new NashornScriptEngineFactory().getScriptEngine("--no-syntax-extensions")
  val reader = new BufferedReader(new InputStreamReader(System.in))
  println(Signals.ready)
  waitForSignal(Signals.go)

  // Used a compiled script here before, stopped working with JDK 10
  var src =
    """(function () {
      |  return 5 + 5;
      |})();
    """.stripMargin

  implicit val ec = ExecutionContext.global

  val futures = (1 to 5).map { _ =>
    Future {
      while (true) {
        scriptEngine.eval(src)
      }
    }
  }

  Await.result(Future.sequence(futures), 30.seconds)
}