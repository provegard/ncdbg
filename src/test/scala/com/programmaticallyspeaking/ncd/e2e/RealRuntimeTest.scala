package com.programmaticallyspeaking.ncd.e2e

import akka.actor.{ActorRef, Inbox, PoisonPill}
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.CallFrame
import com.programmaticallyspeaking.ncd.chrome.domains.{Debugger, Domain, Runtime => RuntimeD}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.testing.{FakeFilePublisher, SharedInstanceActorTesting}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._

trait RealRuntimeTestFixture extends E2ETestFixture with SharedInstanceActorTesting with ScalaFutures with IntegrationPatience {

  var runtime: ActorRef = _
  var debugger: ActorRef = _

  def enableRuntime: Unit = {
    implicit val container = new Container(Seq(FakeFilePublisher, getHost))
    // Reuse the debugger, so create & enable only once.
    if (runtime == null) {
      runtime = newActorInstance[RuntimeD]
      sendRequest(Domain.enable)
    }

    // Debugger is needed as well, for stopping on breakpoints
    if (debugger == null) {
      debugger = newActorInstance[Debugger]
      sendRequestToDebugger(Domain.enable)
    }

    // clean slate
    sendRequestToDebugger(Debugger setBreakpointsActive true)
    sendRequestToDebugger(Debugger setPauseOnExceptions "none")
  }

  def sendRequest(msg: AnyRef): Any = sendRequestAndWait(runtime, msg)
  def sendRequestToDebugger(msg: AnyRef): Any = sendRequestAndWait(debugger, msg)

  protected def withHead(callFrames: Seq[CallFrame])(fun: (CallFrame) => Unit) = {
    callFrames.headOption match {
      case Some(cf) => fun(cf)
      case None => fail("No call frames")
    }
  }

  protected def withScript(callFrames: Seq[CallFrame])(f: (Script) => Unit) = {
    withHead(callFrames) { cf =>
      getHost.findScript(ScriptIdentity.fromId(cf.location.scriptId)) match {
        case Some(s) => f(s)
        case None => fail("Unknown script: " + cf.location.scriptId)
      }
    }
  }

  protected override def stopRunner(): Unit = {
    Seq(debugger, runtime).foreach { actorRef =>
      Option(actorRef).foreach { ar =>
        val inbox = Inbox.create(system)
        inbox.watch(ar)
        inbox.send(ar, PoisonPill)
        // wait a few seconds for the actor to die
        inbox.receive(2.seconds)
      }
    }
    runtime = null
    debugger = null
    super.stopRunner()
  }

  override protected def beforeEachTest(): Unit = enableRuntime

}

class RealRuntimeTest extends RealRuntimeTestFixture with TableDrivenPropertyChecks {

  private def waitForDebugger(testers: Tester*) = {
    runScript("debugger;")(testers: _*)
  }

  "Runtime" - {
    "compileScript" - {
      "reports OK if persist=false for a script that is good" in {
        waitForDebugger(_ => {
          val result = sendRequest(RuntimeD.compileScript("1+2", "", false, None))
          result should be (RuntimeD.CompileScriptResult(null, None))
        })
      }

      "reports a _translated_ error if the script is an incomplete expression (with persist=false)" in {
        waitForDebugger(_ => {
          val result = sendRequest(RuntimeD.compileScript("1+", "", false, None))
          result match {
            case r: RuntimeD.CompileScriptResult =>
              val Some(desc) = r.exceptionDetails.flatMap(_.exception).flatMap(_.description)
              desc should startWith ("SyntaxError: Unexpected end of input")
            case other => fail("unexpected: " + other)
          }
        })
      }
    }
  }
}
