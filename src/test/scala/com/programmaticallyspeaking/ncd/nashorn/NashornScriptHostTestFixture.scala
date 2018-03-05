package com.programmaticallyspeaking.ncd.nashorn

import akka.actor.{ActorRef, Inbox, Props}
import com.programmaticallyspeaking.ncd.host.{ExceptionPauseType, ScriptEvent}
import com.programmaticallyspeaking.ncd.messaging.{Observer, Subscription}
import com.programmaticallyspeaking.ncd.testing.{SharedInstanceActorTesting, UnitTest}
import org.scalatest.concurrent.{AbstractPatienceConfiguration, PatienceConfiguration}
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.{Millis, Seconds, Span}
import org.slf4s.Logging

import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** Provides a patience configuration that has a timeout that is shorter than the default timeout in
  * [[VirtualMachineLauncher]]. This ensures that any ScalaTest wait operations complete/stop before our own.
  */
trait FairAmountOfPatience extends AbstractPatienceConfiguration { this: PatienceConfiguration =>

  private val defaultPatienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = scaled(Span(10, Seconds)),
      interval = scaled(Span(150, Millis))
    )

  implicit abstract override val patienceConfig: PatienceConfig = defaultPatienceConfig
}

object Signals {
  val go = "go"
  val ready = "ready"
  val scriptDone = "script_done"
}

trait VirtualMachineLauncher { self: SharedInstanceActorTesting with Logging =>

  val scriptExecutor: ScriptExecutorBase

  implicit val executionContext: ExecutionContext

  val runVMTimeout: FiniteDuration = 15.seconds
  val resultTimeout: FiniteDuration = 15.seconds
  val scriptDoneTimeout: FiniteDuration = 5.seconds

  // Long timeout for receiving a response from the runner actor. The runner actor uses the timeouts above to send
  // an error on timeout, so we need a wait timeout longer than those.
  private val runnerTimeout: FiniteDuration = 2.minutes

  private var logSubscription: Subscription = _
  private var host: NashornScriptHost = _
  private var runner: ActorRef = _

  protected def javaHome: Option[String] = None

  private var extraArgs: Seq[String] = Seq.empty
  private var runningExtraArgs: Seq[String] = Seq.empty

  protected def useNashornArguments(args: Seq[String]) = {
    extraArgs = args
  }

  protected def stopRunner(): Unit = {
    Option(runner).foreach { r =>
      r ! ScriptExecutorRunner.Stop
    }
    runner = null
    host = null
    runningExtraArgs = Seq.empty
  }

  protected def startRunnerIfNecessary() = {
    if (runner == null) {
      runner = system.actorOf(Props(new ScriptExecutorRunner(scriptExecutor)))
      val inbox = Inbox.create(system)
      inbox.send(runner, ScriptExecutorRunner.Start(javaHome, extraArgs, runVMTimeout))
      inbox.receive(runnerTimeout) match {
        case ScriptExecutorRunner.Started(theHost) =>
          host = theHost
          // Save the extraArgs that are running now
          runningExtraArgs = extraArgs
        case ScriptExecutorRunner.StartError(progress, err) =>
          // The runner should stop itself - clear our ref so that we'll re-create it next time
          runner = null
          throw new RuntimeException("Startup failure: " + progress, err.orNull)
      }
    }
  }

  protected def observeScriptEvents(observer: Observer[ScriptEvent]): Unit = {
    assert(runner != null, "Runner is unset")

    val inbox = Inbox.create(system)
    inbox.send(runner, ScriptExecutorRunner.ObserveScriptEvents(observer))
    inbox.receive(2.seconds)
  }

  protected def executeScript[R](script: String, observer: Observer[ScriptEvent], handler: (NashornScriptHost) => Future[R]): R = {
    assert(runner != null, "Runner is unset")

    if (extraArgs != runningExtraArgs) {
      log.info(s"Restarting VM before test since args ($extraArgs) differs from current ones ($runningExtraArgs).")
      stopRunner()
      startRunnerIfNecessary()
    }

    // If the handle throws, we won't even send the script. The handler returns a Future, and it ought to be safe to
    // invoke it before we pass the script, because it cannot rely on the exact order of things so it must react to
    // events.
    val f = handler(host)

    val inbox = Inbox.create(system)
    inbox.send(runner, ScriptExecutorRunner.ExecuteScript(script, observer, resultTimeout))
    inbox.receive(runnerTimeout) match {
      case ScriptExecutorRunner.ScriptWillExecute =>

        val handlerResult = Try(Await.result(f, resultTimeout))

        host.resume()

        def unexpectedError(reason: String) = {
          stopRunner()
          // If the handler result is ok, we can throw here.
          // If the handler result is an exception, we add this one as suppressed, because the handler exception
          // is the main one.
          handlerResult match {
            case Success(_) => throw new RuntimeException("Unexpected: " + reason)
            case Failure(t: TimeoutException) =>
              // A timeout exception only says "Timed out waiting for...". It's not very interesting, so treat it as
              // secondary.
              val t2 = new RuntimeException("Unexpected: " + reason)
              t2.addSuppressed(t)
              throw t2
            case Failure(t) =>
              t.addSuppressed(new RuntimeException("Unexpected: " + reason))
              throw t
          }
        }

        // Await the final script-done signal
        Try(inbox.receive(runnerTimeout)) match {
          case Success(ScriptExecutorRunner.ScriptExecutionDone) => // Ok!
          case Success(ScriptExecutorRunner.ScriptFailure(reason)) => unexpectedError(reason)
          case other => unexpectedError(other.toString)
        }

        handlerResult match {
          case Success(r) => r
          case Failure(_: TimeoutException) =>
            throw new TimeoutException("Timed out waiting for result. Progress:\n" + summarizeProgress())
          case Failure(_: TestFailedDueToTimeoutException) =>
            throw new TimeoutException("Timed out waiting for result. Progress:\n" + summarizeProgress())
          case Failure(t) => throw t
        }

      case ScriptExecutorRunner.ScriptFailure(reason) =>
        throw new RuntimeException("Script cannot be executed: " + reason)
    }
  }

  protected def summarizeProgress(): String = {
    Option(runner).map { r =>
      val inbox = Inbox.create(system)
      inbox.send(r, ScriptExecutorRunner.GetProgress)
      inbox.receive(1.second) match {
        case ScriptExecutorRunner.ProgressResponse(p) => p
        case other => throw new RuntimeException("Unexpected: " + other)
      }
    }.getOrElse("(progress not available)")
  }

  override def afterAllTests(): Unit = stopRunner()

  protected def getHost = Option(host).getOrElse {
    startRunnerIfNecessary()
    if (host == null) throw new RuntimeException("Failed to start runner & get host instance")
    host
  }
}

trait NashornScriptHostTestFixture extends UnitTest with Logging with SharedInstanceActorTesting with VirtualMachineLauncher {
  implicit val executionContext: ExecutionContext

  override val scriptExecutor: ScriptExecutorBase = ScriptExecutor

  private val seenScripts = mutable.Set[String]()


  protected def observeAndRunScriptSync[R](script: String, observer: Observer[ScriptEvent])(handler: (NashornScriptHost) => R): Unit = {
    observeAndRunScriptAsync(script, observer) { host => Future { handler(host) }}
  }

  protected def beforeEachTest(): Unit = {}

  protected def observeAndRunScriptAsync[R](script: String, observer: Observer[ScriptEvent] = null, beforeTest: (NashornScriptHost) => Unit = _ => {})(handler: (NashornScriptHost) => Future[R]): Unit = {
    // if we have seen this script before, restart the VM to avoid test dependencies due to script reuse
    if (seenScripts.contains(script)) {
      log.info("Restarting VM before test since script has been seen before.")
      stopRunner()
      // Seen scripts are no longer relevant - only per VM
      seenScripts.clear()
    } else seenScripts += script

    val host = getHost

    // Reset things
    host.setSkipAllPauses(false)
    host.pauseOnExceptions(ExceptionPauseType.None)
    host.pauseOnBreakpoints()

    // First let the implementing test class do any setup. RealDebuggerTest enables the Debugger actor, for example.
    beforeEachTest()

    // Call any test-specific before code.
    beforeTest(host)

    executeScript(script, observer, handler)
  }
}