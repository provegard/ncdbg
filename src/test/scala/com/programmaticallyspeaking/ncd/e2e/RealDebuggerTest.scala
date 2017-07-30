package com.programmaticallyspeaking.ncd.e2e

import akka.actor.{ActorRef, PoisonPill}
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.{CallFrame, EvaluateOnCallFrameResult, Location}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{CallArgument, RemoteObject}
import com.programmaticallyspeaking.ncd.chrome.domains.{Debugger, Domain, Runtime => RuntimeD}
import com.programmaticallyspeaking.ncd.host.{ScriptIdentity, ScriptLocation}
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.testing.{FakeFilePublisher, SharedInstanceActorTesting}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.{Future, TimeoutException}

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

    // clean slate
    sendRequest(Debugger setBreakpointsActive true)
    sendRequest(Debugger setPauseOnExceptions "none")
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
          getHost.findScript(ScriptIdentity.fromId(cf.location.scriptId)) match {
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
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (1) // 0-based
        }
      }, callFrames => {
        // Consume the last debugger statement
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (2) // 0-based
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
          |debugger;       // after restart-frame-resume, we should NOT get here
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          // At 'debugger', set the initial breakpoint
          val scriptUrl = getHost.findScript(ScriptIdentity.fromId(cf.location.scriptId)).map(_.url.toString).getOrElse(
            throw new IllegalArgumentException("No script with ID: " + cf.location.scriptId))
          sendRequest(Debugger.setBreakpointByUrl(1, scriptUrl, None, None))
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          // At the regular breakpoint
          sendRequest(Debugger.restartFrame(cf.callFrameId))
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (1) // 0-based
        }
      }, callFrames => {
        // Consume the last debugger statement
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (6) // 0-based
        }
      })
    }

    "should support continuing to a specific location" in {
      enableDebugger
      val script =
        """debugger;
          |var x = 0;
          |x = x + 1;
          |x = x.toString(); // target
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          val scriptId = cf.location.scriptId
          sendRequest(Debugger.continueToLocation(Location(scriptId, 3, None)))
        }
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (3) // 0-based
        }
      })
    }

    "should support continuing to a specific location with column 0 (because that's what Chrome sends)" in {
      enableDebugger
      val script =
        """debugger;
          |if (true) {
          |  var x = 0;
          |  x = x + 1;
          |  x = x.toString(); // target
          |}
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          val scriptId = cf.location.scriptId
          sendRequest(Debugger.continueToLocation(Location(scriptId, 4, Some(0))))
        }
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (4) // 0-based
        }
      })
    }

    "should not leave an unwanted breakpoint after continuing to a location" in {
      enableDebugger
      val script =
        """debugger;
          |var x = 0;
          |while (x < 2) {
          |  x = x + 1; // target
          |}
          |debugger; // end up here afterwards
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          val scriptId = cf.location.scriptId
          sendRequest(Debugger.continueToLocation(Location(scriptId, 3, None)))
        }
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (3) // 0-based
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (5) // 0-based
        }
      })
    }

    "should be able to continue to a location where there already is a breakpoint" in {
      enableDebugger
      val script =
        """debugger;
          |var x = 0;
          |while (x < 2) {
          |  x = x + 1; // target, but also where we set a breakpoint
          |}
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          val scriptId = cf.location.scriptId
          // Debugger API doesn't allow us to set a breakpoint by ID, so we have to access ScriptHost directly.
          getHost.setBreakpoint(ScriptIdentity.fromId(scriptId), ScriptLocation(4, None), None).getOrElse(throw new Exception("Failed to set a breakpoint"))
          sendRequest(Debugger.continueToLocation(Location(scriptId, 3, None)))
        }
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          // The result of continuing to the location
          cf.location.lineNumber should be (3) // 0-based
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          // The original breakpoint
          cf.location.lineNumber should be (3) // 0-based
        }
      })
    }

    def pauseAtNext(script: String, runAtDebugger: () => Unit, possiblePauseLineNumbers: Seq[Int]) = {
      assert(script.contains("done"), "expected the script to contain a done variable")
      val lineOf2ndDebugger = script.split("\r?\n").toSeq.zipWithIndex.filter(_._1.trim == "debugger;").tail.toList match {
        case x :: _ => x._2
        case Nil => fail("No 2nd debugger statement")
      }
      runScript(script)(_ => {
        runAtDebugger()
        sendRequest(Debugger.resume)
        sendRequest(Debugger.pause)
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          // Don't know exactly where it'll pause, so be permissive
          possiblePauseLineNumbers should contain (cf.location.lineNumber)
          // Break out of the loop. Since f captures done, we should always be able to set it on scope 0.
          sendRequest(Debugger.setVariableValue(0, "done", CallArgument(Some(true), None, None), cf.callFrameId))
        }
      }, callFrames => {
        // Consume the last debugger statement, a.k.a. wait for the loop to be finished
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (lineOf2ndDebugger)
        }
      })
    }

    "should support pausing at next statement" - {
      // This script is likely to trigger the method-entry case, and since f is called repeatedly, the method-entry
      // breakpoint will eventually hit.
      val script =
        """var done = false;
          |f(); // compile f
          |debugger;
          |while (!done) {
          |  f();
          |}
          |debugger;
          |function f() {
          |  java.lang.Thread.sleep(done ? 100 : 100); // capture 'done'
          |}
        """.stripMargin

      "when breakpoints are enabled" in {
        enableDebugger
        pauseAtNext(script, () => (), Seq(3, 4, 8))
      }
      "when breakpoints are disabled" in {
        enableDebugger
        pauseAtNext(script, () => Debugger setBreakpointsActive false, Seq(3, 4, 8))
      }
    }

    "should support pausing at next statement when there's no function call involved and the thread isn't sleeping" in {
      // This script will hit the case where we set breakpoints in the current stack frame.
      val script =
        """var done = false, i = 0;
          |debugger;
          |while (!done) {
          |  i = i + 1;
          |}
          |debugger;
        """.stripMargin

      enableDebugger
      pauseAtNext(script, () => (), Seq(2, 3))
    }

    "should support pausing at next statement when the thread is sleeping in a function that won't be called again" in {
      // With method-entry/breakpoint separation, this TC typically hits the method-entry case but since f won't
      // be called again, the method-entry breakpoint is never hit. The TC fails when run individually, but not as part
      // of the suite... :-(
      val script =
        """var done = false, i = 0;
          |f(); // compile
          |debugger;
          |f();
          |while (!done) {
          |  i = i + 1;
          |}
          |debugger;
          |function f() {
          |  java.lang.Thread.sleep(200);
          |}
        """.stripMargin

      enableDebugger
      pauseAtNext(script, () => (), Seq(3, 4, 5, 9))
    }



    "should pause on exception when enabled even if pausing on breakpoint is disabled" in {
      enableDebugger
      val script =
        """debugger; // disable breakpoint pausing here
          |debugger; // this one should be ignored
          |try {
          |  throw new Error("oops"); // we should pause here
          |} catch (e) {
          |  e.toString();
          |}
        """.stripMargin

      runScript(script)(_ => {
        sendRequest(Debugger setBreakpointsActive false)
        sendRequest(Debugger setPauseOnExceptions "all")
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (3)
        }
      })
    }

    "should step even if pausing on breakpoint is disabled" in {
      enableDebugger
      val script =
        """var i = 0;
          |debugger;  // disable breakpoint pausing here, then step
          |i = i + 1; // we should end up here
        """.stripMargin

      runScript(script)(_ => {
        sendRequest(Debugger setBreakpointsActive false)
        sendRequest(Debugger stepOver)
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (2)
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

  protected override def restart(): Unit = {
    Option(debugger).foreach(_ ! PoisonPill)
    debugger = null
    super.restart()
  }

  protected override def runScript(script: String)(testers: Tester*): Unit = {
    try super.runScript(script)(testers: _*) catch {
      case ex: TimeoutException =>
        // The VM may have entered an infinite loop, so we need a new starting point.
        restart()
        throw ex
    }
  }
}
