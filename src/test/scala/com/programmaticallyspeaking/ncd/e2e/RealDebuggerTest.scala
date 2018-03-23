package com.programmaticallyspeaking.ncd.e2e

import akka.actor.{ActorRef, Inbox, PoisonPill}
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.{CallFrame, EvaluateOnCallFrameResult, Location}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{CallArgument, RemoteObject}
import com.programmaticallyspeaking.ncd.chrome.domains.{Debugger, Domain, Runtime => RuntimeD}
import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.host.{ComplexNode, Script, ScriptIdentity, ScriptLocation}
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.nashorn.types.ObjectPropertyDescriptorTest
import com.programmaticallyspeaking.ncd.testing.{FakeFilePublisher, SharedInstanceActorTesting}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._
import scala.util.Success

trait RealDebuggerTestFixture extends E2ETestFixture with SharedInstanceActorTesting with ScalaFutures with IntegrationPatience {

  var debugger: ActorRef = _

  def enableDebugger: Unit = {
    // Reuse the debugger, so create & enable only once.
    if (debugger == null) {
      implicit val container = new Container(Seq(FakeFilePublisher, getHost))
      debugger = newActorInstance[Debugger]
      sendRequest(Domain.enable)
    }

    // clean slate
    sendRequest(Debugger setBreakpointsActive true)
    sendRequest(Debugger setPauseOnExceptions "none")
  }

  def sendRequest(msg: AnyRef): Any = sendRequestAndWait(debugger, msg)

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
    Option(debugger).foreach { actorRef =>
      val inbox = Inbox.create(system)
      inbox.watch(actorRef)
      inbox.send(actorRef, PoisonPill)
      // wait a few seconds for the actor to die
      inbox.receive(2.seconds)
    }
    debugger = null
    super.stopRunner()
  }

  override protected def beforeEachTest(): Unit = enableDebugger

}

class RealDebuggerTest extends RealDebuggerTestFixture with TableDrivenPropertyChecks {


  "Debugging" - {
    "supports getting object properties twice from a second thread when the first is no longer usable" in {
      val script =
        """
          |var Executors = Java.type("java.util.concurrent.Executors");
          |var e1 = Executors.newSingleThreadExecutor();
          |var e2 = Executors.newSingleThreadExecutor();
          |
          |var Runnable = Java.type('java.lang.Runnable');
          |var func = Java.extend(Runnable, {
          |    run: function() {
          |        var obj = { foo: 42 };
          |        debugger;
          |        obj.toString();
          |    }
          |});
          |
          |var f1 = e1.submit(new func());
          |f1.get();
          |var f2 = e2.submit(new func());
          |f2.get();
          |
        """.stripMargin

      def getPropNames(objName: String, callFrame: CallFrame): Seq[String] = {
        getHost.evaluateOnStackFrame(callFrame.callFrameId, objName, Map.empty) match {
          case Success(c: ComplexNode) =>
            getHost.getObjectProperties(c.objectId, true, false).map(_._1)
          case other => fail("" + other)
        }
      }

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (9)

          // Get properties on the first thread, trigger creation of the extractor function
          getPropNames("obj", cf) should be (Seq("foo", "__proto__"))
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be(9)

          // Get properties on the second thread, extractor function cannot operate on the first (dead) one
          getPropNames("obj", cf) should be (Seq("foo", "__proto__"))
        }
      })
    }

    "should handle stepping over a line with a breakpoint" in {
      val script =
        """
          |var i = 0;
          |debugger;     // here, we set a breakpoint on +2 lines below, then step
          |i++;          // step over from here
          |i++;          // here is the breakpoint, step over from here
          |i.toString(); // should end up here
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be (2)
          withScript(callFrames) { s =>
            sendRequest(Debugger.setBreakpointByUrl(4, Some(s.url.toString), None, Some(0), None))
          }
          sendRequest(Debugger.stepOver)
        }
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be(3)
          sendRequest(Debugger.stepOver)
        }
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be(4)
          sendRequest(Debugger.stepOver)
        }
        DontAutoResume
      }, callFrames => {
        withHead(callFrames) { cf =>
          cf.location.lineNumber should be(5)
        }
      })
    }

    "should support setVariableValue" in {
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
          val r2 = sendRequest(Debugger.evaluateOnCallFrame(cf.callFrameId, "y", None, None))
          r2 should be(EvaluateOnCallFrameResult(RemoteObject.forNumber(42)))
        }
      })
    }

    "should set a variable in the correct stack frame" in {
      // Note that the starting point 1.0 instead of 1 is important, since otherwise Nashorn will first attempt
      // an Integer-arged method, but will then recompile into a Double-arged method upon the next call, and the
      // different recompilations will result in unique locations which will cause the flawed Location-based stack
      // lookup to actually work...
      val script =
        """function f(callNo) {
          |  var x = 0;
          |  if (callNo === 3) {
          |    debugger;
          |    return x;
          |  }
          |  var v = 2 * f(callNo + 1);
          |  return v + x;
          |}
          |var result = f(1.0);
          |debugger;
        """.stripMargin

      runScript(script)(callFrames => {
        // At debugger statement
        // Set x to 1 in the local scope in the grandparent stack frame (stack frame 3/3).
        // The flawed Location-based stack frame lookup will hit frame 2/3 instead, since it has the same Location
        // as frame 3/3.
        callFrames.drop(2).headOption match {
          case Some(cf) =>
            cf.scopeChain.zipWithIndex.find(_._1.`type` == "local") match {
              case Some((_, scopeIdx)) =>
                sendRequest(Debugger.setVariableValue(scopeIdx, "x", RuntimeD.CallArgument(Some(1), None, None), cf.callFrameId))

              case None => fail("No local scope")
            }
          case None => fail("No parent stack frame")
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          // result is:
          // if changed in stack frame 1/3: 100 = 4
          // if changed in stack frame 2/3: 010 = 2
          // if changed in stack frame 3/3: 001 = 1
          sendRequest(Debugger.evaluateOnCallFrame(cf.callFrameId, "result", None, None)) match {
            case EvaluateOnCallFrameResult(RemoteObject("number", _, _, _, Some(numValue), _, _, _), _) =>
              // Java 9 returns an int, Java 9 a double
              numValue.toString.toDouble should be (1.0d)
            case other => fail("Unexpected result: " + other)
          }
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
            val r2 = sendRequest(Debugger.evaluateOnCallFrame(cf.callFrameId, "this.result", None, None))
            r2 should be(EvaluateOnCallFrameResult(expected))
          }
        })
      }

    }

    "should support a conditional breakpoint (with column number)" in {
      val script =
        """debugger;
          |var list = [];
          |for (var i = 0; i < 2; i++) {
          |  list.push(i);
          |}
        """.stripMargin

      runScript(script)(callFrames => {
        withScript(callFrames) { s =>
          sendRequest(Debugger.setBreakpointByUrl(3, Some(s.url.toString), None, Some(2), Some("i>0")))
        }
      }, callFrames => {
        withHead(callFrames) { cf =>
          val r2 = sendRequest(Debugger.evaluateOnCallFrame(cf.callFrameId, "i", None, None))
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

    // Test disabled due to https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8187143
    // Fails with Java 1.8u151 and up.
    "should support frame restart when paused at a regular breakpoint" ignore {
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
        withScript(callFrames) { s =>
          sendRequest(Debugger.setBreakpointByUrl(1, Some(s.url.toString), None, None, None))
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
      val script =
        """debugger;
          |var x = 0;
          |while (x < 2) {
          |  x = x + 1; // continue to here, but there's also a normal breakpoint
          |}
        """.stripMargin

      runScript(script)(callFrames => {
        withHead(callFrames) { cf =>
          val scriptId = cf.location.scriptId
          // Debugger API doesn't allow us to set a breakpoint by ID, so we have to access ScriptHost directly.
          getHost.setBreakpoint(ScriptIdentity.fromId(scriptId), ScriptLocation(4, None), None) // 1-based line
          sendRequest(Debugger.continueToLocation(Location(scriptId, 3, None)))                 // 0-based line
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
        pauseAtNext(script, () => (), Seq(3, 4, 8))
      }
      "when breakpoints are disabled" in {
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

      pauseAtNext(script, () => (), Seq(3, 4, 5, 9))
    }



    "should pause on exception when enabled even if pausing on breakpoint is disabled" in {
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
}
