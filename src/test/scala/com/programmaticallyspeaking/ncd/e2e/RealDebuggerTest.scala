package com.programmaticallyspeaking.ncd.e2e

import akka.actor.ActorRef
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.{CallFrame, EvaluateOnCallFrameResult}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.chrome.domains.{Debugger, Domain, Runtime => RuntimeD}
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.testing.{FakeFilePublisher, FreeActorTesting}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.concurrent.Future

class RealDebuggerTest extends E2ETestFixture with FreeActorTesting with ScalaFutures with IntegrationPatience {

  var initDoneFuture: Future[Unit] = _
  var debugger: ActorRef = _

  def enableDebugger: Unit = {
    // Reuse the debugger, so create & enable only once.
    if (debugger == null) {
      implicit val container = new Container(Seq(FakeFilePublisher, getHost))
      debugger = newActorInstance[Debugger]
      sendRequestAndWait(debugger, Domain.enable)
    }
  }

  def sendRequest(msg: AnyRef): Any = sendRequestAndWait(debugger, msg)

  "should support setVariableValue" in {
    enableDebugger
    val script =
      """
        |this.x = 5;
        |var f = function () {
        |  var x = 6;
        |  debugger;
        |};
        |var f2 = function () {
        |  var y = x;
        |  debugger;
        |  y.toString();
        |}
        |f();
        |f2();
      """.stripMargin

    runScript(script)(callFrames => {
      withHead(callFrames) { cf =>
        // In function f, scope 0 should be global
        sendRequest(Debugger.setVariableValue(0, "x", RuntimeD.CallArgument(Some(42), None, None), cf.callFrameId))
      }
    }, callFrames => {
      withHead(callFrames) { cf =>
        // In function f2, y should have value 42
        val r2 = sendRequest(Debugger.evaluateOnCallFrame(cf.callFrameId, "y", None, None, None))
        r2 should be(EvaluateOnCallFrameResult(RemoteObject.forNumber(42)))
      }
    })
  }

  "should support setVariableValue when a local variable is targeted" in {
    enableDebugger
    val script =
      """
        |var f = function (username) {
        |  debugger;
        |  return "Hello " + username;
        |};
        |this.greeting = f("foo");
        |debugger;
      """.stripMargin

    runScript(script)(callFrames => {
      withHead(callFrames) { cf =>
        // Get the local scope
        cf.scopeChain.zipWithIndex.find(_._1.`type` == "local") match {
          case Some((_, idx)) =>
            sendRequest(Debugger.setVariableValue(idx, "username", RuntimeD.CallArgument(Some("bar"), None, None), cf.callFrameId))
          case None => fail("No local scope")
        }
      }
    }, callFrames => {
      withHead(callFrames) { cf =>
        val r2 = sendRequest(Debugger.evaluateOnCallFrame(cf.callFrameId, "this.greeting", None, None, None))
        r2 should be(EvaluateOnCallFrameResult(RemoteObject.forString("Hello bar")))
      }
    })
  }

  "should support a conditional breakpoint" in {
    enableDebugger
    val script =
      """debugger;
        |var list = [];
        |for (var i = 0; i < 2; i++) {
        |  list.push(i);
        |}
      """.stripMargin

    runScript(script)(callFrames => {
      withHead(callFrames) { cf =>
        getHost.scriptById(cf.location.scriptId) match {
          case Some(s) =>
            sendRequest(Debugger.setBreakpointByUrl(3, s.url.toString, 3, Some("i>0")))
          case None => fail("Unknown script: " + cf.location.scriptId)
        }
      }
    }, callFrames => {
      withHead(callFrames) { cf =>
        val r2 = sendRequest(Debugger.evaluateOnCallFrame(cf.callFrameId, "i", None, None, None))
        // We may/will get i==1.0, so the assert becomes somewhat complicated.
        r2 match {
          case EvaluateOnCallFrameResult(ro, None) =>
            ro.value.map(_.toString.toDouble.toInt) should be (Some(1))
          case other => fail("Unexpected result: " + other)
        }
      }
    })
  }

  private def withHead(callFrames: Seq[CallFrame])(fun: (CallFrame) => Unit) = {
    callFrames.headOption match {
      case Some(cf) => fun(cf)
      case None => fail("No call frames")
    }
  }
}
