package com.programmaticallyspeaking.ncd.e2e

import akka.actor.ActorRef
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.{CallFrame, EvaluateOnCallFrameResult}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.chrome.domains.{Debugger, Domain, Runtime => RuntimeD}
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.testing.{FakeFilePublisher, SharedInstanceActorTesting}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.Future

class RealDebuggerTest extends E2ETestFixture with SharedInstanceActorTesting with ScalaFutures with IntegrationPatience with TableDrivenPropertyChecks {

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

  "Debugging" - {
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

    val setVariableValueCases = Table(
      ("type", "initial", "toSet", "expected"),
      ("string", "'foo'", "bar", RemoteObject.forString("bar")),
      ("int", "5", 6, RemoteObject.forNumber(6)),
      ("double", "5.5", 6.6, RemoteObject.forNumber(6.6)),
      ("bool", "true", false, RemoteObject.falseValue)
    )

    forAll(setVariableValueCases) { (typ, initial, toSet, expected) =>
      s"should support setVariableValue when a local variable of type $typ is targeted" in {
        enableDebugger
        val script =
          s"""
             |var f = function (arg) {
             |  debugger;
             |  return arg;
             |};
             |this.result = f($initial);
             |debugger;
           """.stripMargin

        runScript(script)(callFrames => {
          withHead(callFrames) { cf =>
            // Get the local scope
            cf.scopeChain.zipWithIndex.find(_._1.`type` == "local") match {
              case Some((_, idx)) =>
                sendRequest(Debugger.setVariableValue(idx, "arg", RuntimeD.CallArgument(Some(toSet), None, None), cf.callFrameId))
              case None => fail("No local scope")
            }
          }
        }, callFrames => {
          withHead(callFrames) { cf =>
            val r2 = sendRequest(Debugger.evaluateOnCallFrame(cf.callFrameId, "this.result", None, None, None))
            r2 should be(EvaluateOnCallFrameResult(expected))
          }
        })
      }

    }

    "should support a conditional breakpoint (with column number)" in {
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
              sendRequest(Debugger.setBreakpointByUrl(3, s.url.toString, Some(2), Some("i>0")))
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

    "should support frame restart when paused at a debugger statement" in {
      enableDebugger
      val script =
        """var f = function () {
          |  debugger; // stop here
          |  debugger; // after restart + resume, we should NOT get here
          |};
          |f();
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          sendRequest(Debugger.restartFrame(cf.callFrameId))
          sendRequest(Debugger.resume)
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (1) // 0-based
        }
      })
    }

    "should support frame restart when paused at a regular breakpoint" in {
      enableDebugger
      val script =
        """var f = function () {
          |  f.toString(); // regular breakpoint here
          |};
          |f();            // make sure f is compiled
          |debugger;       // where we set a breakpoint
          |f();
          |debugger;       // after resume, we should NOT get here
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          // At 'debugger', set the initial breakpoint
          reportProgress("Setting the initial breakpoint in function f")
          val scriptUrl = getHost.scriptById(cf.location.scriptId).map(_.url.toString).getOrElse(
            throw new IllegalArgumentException("No script with ID: " + cf.location.scriptId))
          sendRequest(Debugger.setBreakpointByUrl(1, scriptUrl, None, None))
          reportProgress("Resuming after setting the initial breakpoint")
          sendRequest(Debugger.resume)
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          // At the regular breakpoint
          reportProgress(s"Restarting frame with ID ${cf.callFrameId}")
          sendRequest(Debugger.restartFrame(cf.callFrameId))
          reportProgress("Resuming after restarting the frame")
          sendRequest(Debugger.resume)
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (1) // 0-based
        }
      })
    }

  }

  private def withHead(callFrames: Seq[CallFrame])(fun: (CallFrame) => Unit) = {
    callFrames.headOption match {
      case Some(cf) => fun(cf)
      case None => fail("No call frames")
    }
  }
}
