package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import org.scalactic.Equality
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.mutable.ListBuffer
import scala.concurrent.Promise
import scala.util.Try

class BreakpointTest extends BreakpointTestFixture with TableDrivenPropertyChecks {

  val scopeTests = Table(
    ("desc", "script", "expectation"),
    ("on global level", "debugger;", Seq("Global:.*")),
    ("on global level with variable",
      """var globalVar = 0;
        |debugger;
        |globalVar.toString()
      """.stripMargin, Seq("Global:.*globalVar.*Uint16Array.*")),
    ("inside a function with a local variable",
      """(function () {
        |  var x = 0;
        |  debugger;
        |  x.toString();
        |})();
      """.stripMargin, Seq("Local:x", "Global:.*")),
    // TODO: Figure out how to see the 'foo' variable in the 'with' block.
    ("inside a with block inside a function",
      """(function () {
        |  var x = 0;
        |  var obj = { foo: 42 };
        |  with (obj) {
        |    debugger;
        |    x.toString(); // capture something from the surrounding scope
        |  }
        |})();
      """.stripMargin, Seq("Local:obj", "With:", "Closure:x", "Global:.*")),
    ("inside a function inside a function",
      """(function () {
        |  var x = 0;
        |  (function () {
        |    var y = 0;
        |    debugger;
        |    y.toString();
        |    x.toString(); // capture something from the parent function
        |  })();
        |})();
      """.stripMargin, Seq("Local:y", "Closure:x", "Global:.*")),
    ("inside a function inside a 'with' block inside a function",
      """(function () {
        |  var x = 0;
        |  var obj = { foo: 42 };
        |  with (obj) {
        |    (function () {
        |      var y = 0;
        |      debugger;
        |      y.toString();
        |      x.toString(); // capture something from the parent function
        |    })();
        |  }
        |})();
      """.stripMargin, Seq("Local:y", "With:", "Closure:x", "Global:.*")),
    ("inside a function where this != global",
      """(function () {
        |  var x = 0;
        |  debugger;
        |  x.toString();
        |}).call({});
      """.stripMargin, Seq("Local:x", "Global:.*")),
    ("inside a scope-less function with this === null",
      """(function () {
        |  debugger;
        |}).call(null);
      """.stripMargin, Seq("Global:.*"))
  )

  implicit val regexpEq = new Equality[Seq[String]] {
    override def areEqual(a: Seq[String], b: Any): Boolean = b match {
      case regexpes: Seq[_] =>
        a.size == regexpes.size && a.zip(regexpes).forall { case (str, regexp) =>
          regexp.toString.r.pattern.matcher(str).matches
        }

      case _ => false
    }
  }

  "setting a breakpoint for an unknown script" - {
    "works and returns no locations" in {
      waitForBreakpoint("debugger;") { (host, _) =>
        val bp = host.setBreakpoint(URLRegexBasedScriptIdentity(".*unknown\\.js$"), ScriptLocation(1, None), None)
        bp.locations should be (Seq.empty)
      }
    }

    "results in a BreakpointResolved event later on" in {
      waitForBreakpointThenEvent("debugger;") { (host, hb) =>
        host.ignoreBreakpoints()
        host.setBreakpoint(URLRegexBasedScriptIdentity(".*unknown\\.js$"), ScriptLocation(1, None), None)

        val loader =
          """
            |load({
            |  script: "'use strict';",
            |  name: "unknown.js"
            |});
          """.stripMargin
        host.evaluateOnStackFrame(hb.stackFrames.head.id, loader, Map.empty)
      } {
        case BreakpointResolved(_, location) =>
          location.location.lineNumber1Based should be (1)
      }
    }

    "actually breaks in a new script" in {
      waitForBreakpoints("debugger; this.ff();")({ (host, hb) =>
        host.setBreakpoint(URLRegexBasedScriptIdentity(".*unknown2\\.js$"), ScriptLocation(2, None), None)

        val loader =
          """
            |load({
            |  script: "this.ff = function(){\nreturn 42;\n}",
            |  name: "unknown2.js"
            |});
            |print("ff = " + this.ff);
          """.stripMargin
        host.evaluateOnStackFrame(hb.stackFrames.head.id, loader, Map.empty)
      }, { (host, hb) =>
        hb.stackFrames.head.scriptURL.toString should include ("unknown2.js")
      })
    }
  }

  "when a breakpoint is hit" - {
    "the scopes should be sorted out" - {
      forAll(scopeTests) { (desc, script, expectationRegExps) =>
        desc in {
          waitForBreakpoint(script) { (host, breakpoint) =>
            breakpoint.stackFrames.headOption match {
              case Some(st) =>

                st.scopeChain.map(s => describeScope(host, s)) should equal (expectationRegExps)

              case None => fail("no stack frames!")
            }
          }
        }
      }
    }

    "the (1-based) column number is reported" in {
      val script =
        """(function () {
          |    debugger; // 4 spaces -> 'd' is at position 5
          |})();
        """.stripMargin
      waitForBreakpoint(script) { (_, breakpoint) =>
        breakpoint.stackFrames.headOption.flatMap(_.location.columnNumber1Based) should be (Some(5))
      }
    }

    "for a debugger statement" - {
      lazy val breakpoint = {
        val script = "debugger;"
        var theBp: HitBreakpoint = null
        waitForBreakpoint(script) { (_, bp) =>
          theBp = bp
        }
        theBp
      }

      "the reason is Debugger" in {
        breakpoint.reason should be (BreakpointReason.Debugger)
      }

      "there is no breakpoint ID" in {
        breakpoint.breakpointId should be ('empty)
      }
    }

    "for a regular breakpoint" - {
      lazy val breakpoint = {
        val script = "while (false) {}"
        var theBp: HitBreakpoint = null
        breakAtLine(script, 1) { (_, bp) => theBp = bp }
        theBp
      }

      "the reason is Breakpoint" in {
        breakpoint.reason should be (BreakpointReason.Breakpoint)
      }

      "there is a breakpoint ID" in {
        breakpoint.breakpointId should be ('defined)
      }
    }

    "a local scope" - {
      def evaluateLocalScopeObject(script: String)(handler: (ScriptHost, ComplexNode) => Unit): Unit = {
        waitForBreakpoint(script) { (host, breakpoint) =>
          breakpoint.stackFrames.headOption.flatMap(st => st.scopeChain.find(_.scopeType == ScopeType.Local)) match {
            case Some(scope) =>
              scope.value match {
                case c: ComplexNode =>
                  handler(host, c)
                case other => fail("Scope is not a ComplexNode: " + other)
              }

            case None => fail("no stack frames or local scope!")
          }
        }
      }

      "with a function argument" - {
        val script =
          """(function (arg) {
            |  debugger;
            |  arg.toString();
            |})("test");
          """.stripMargin

        "should make a variable (argument) writable" in {
          evaluateLocalScopeObject(script) { (host, scopeObj) =>
            host.getObjectProperties(scopeObj.objectId, true, false).toMap.get("arg") match {
              case Some(propDesc) =>
                propDesc.isWritable should be (true)
              case None => fail("No property named 'arg'")
            }
          }
        }

        "should publish a variable (argument) as a data property with a value" in {
          evaluateLocalScopeObject(script) { (host, scopeObj) =>
            host.getObjectProperties(scopeObj.objectId, true, false).toMap.get("arg") match {
              case Some(propDesc) =>
                propDesc.value should be (Some(SimpleValue("test")))
              case None => fail("No property named 'arg'")
            }
          }
        }

        "should not leak the anonymous 'obj' object when getting all properties (not only own)" in {
          evaluateLocalScopeObject(script) { (host, scopeObj) =>
            host.getObjectProperties(scopeObj.objectId, false, false).toMap.keys should not contain ("obj")
          }
        }
      }

      "in a peculiar situation that I cannot really describe" - {
        val script =
          """
            |(function() {
            |  var bar, fun, number;
            |
            |  number = 0;
            |  bar = new Int8Array([21, 31]); // this is required, don't know why
            |
            |  fun = function() {
            |    var sub = 42;
            |    number += 1; // must capture closure
            |    debugger;
            |    sub.toString();
            |  };
            |
            |  this.get = fun;
            |
            |}).call(this);
            |
            |this.get();
          """.stripMargin

        "should support property extraction from the local scope object" in {
          evaluateLocalScopeObject(script) { (host, scopeObj) =>
            host.getObjectProperties(scopeObj.objectId, false, false).toMap.keys should contain ("sub")
          }
        }

        "should not include closure variables in the local scope, even if not-own properties are requested, because that is how Chrome works, it seems..." in {
          evaluateLocalScopeObject(script) { (host, scopeObj) =>
            host.getObjectProperties(scopeObj.objectId, false, false).toMap.keys should not contain ("number")
          }
        }
      }
    }
  }

  "given breakable locations" - {
    def createObserver(handler: ((MultiBreakpointsData) => Unit)): Observer[ScriptEvent] = {
      var breakpointsHitSoFar = 0
      val observer = Observer.from[ScriptEvent] {
        case bp: HitBreakpoint =>
          breakpointsHitSoFar += 1
          handler(MultiBreakpointsData(bp, breakpointsHitSoFar))
      }
      observer
    }

    def testSetBreakpoint(script: String, line: Int, col: Option[Int])(handler: (Option[Breakpoint]) => Unit): Unit = {
      val donePromise = Promise[Unit]()
      val observer = createObserver { data =>
        val maybeBreakpoint = setBreakpoint(data.bp, line, col)

        // Remove the breakpoint right away, so we won't hit it
        maybeBreakpoint.foreach(b => getHost.removeBreakpointById(b.breakpointId))
        donePromise.complete(Try(handler(maybeBreakpoint)))
      }
      observeAndRunScriptAsync(script, observer) { _ =>
        donePromise.future
      }
    }

    def columnNumbers(bp: Option[Breakpoint]) = bp.map(_.locations.flatMap(_.location.columnNumber1Based)).getOrElse(Seq.empty)

    "in a script with a 'with' statement last in a function" - {
      val script =
        """(function () {
          |  debugger;
          |  var obj = {};
          |  with (obj) {
          |    obj.toString();
          |  }
          |})();
        """.stripMargin

      "only a single location is returned for the 'with' line" in {
        testSetBreakpoint(script, 4, None) { bp =>
          columnNumbers(bp) should be(Seq(3))
        }
      }
    }

    "in a script with multiple on the same line" - {
      val script =
        """function fun() {
          |  var foo = function() { return 42; } // multiple breakable here
          |  return foo;
          |}
          |fun()();  // ensure compilation of fun and foo
          |debugger; // where we will set breakpoints
          |fun()();  // two calls, should hit two breakpoints...
        """.stripMargin

      "setting a breakpoint with no column doesn't try to guess column numbers anymore" in {
        testSetBreakpoint(script, 2, None) { bp =>
          columnNumbers(bp) should be (Seq(3))
        }
      }

      "setting a breakpoint with a column returns a single location" in {
        testSetBreakpoint(script, 2, Some(3)) { bp =>
          columnNumbers(bp) should be (Seq(3))
        }
      }

      "setting a breakpoint with an incorrect column returns breakpoints, because of inaccurate source maps" in {
        testSetBreakpoint(script, 2, Some(1)) { bp =>
          columnNumbers(bp) should be (Seq(3))
        }
      }

      "a breakpoint set for all locations on that line will get hit twice" in {
        val donePromise = Promise[Unit]()
        val breakpointIds = ListBuffer[String]()
        val observer = createObserver { data =>
          val scriptId = data.bp.stackFrames.head.scriptId
          val breakpointId = data.bp.breakpointId

          if (data.hitsSoFar == 1) {
            // debugger
            getHost.findScript(ScriptIdentity.fromId(scriptId)).map { s =>
              getHost.setBreakpoint(ScriptIdentity.fromURL(s.url), ScriptLocation(2, None), None)
            } match {
              case Some(bps) => breakpointIds += bps.breakpointId
              case None => donePromise.failure(new Exception("No script or breakpoint"))
            }
          } else breakpointIds ++= breakpointId

          if (data.hitsSoFar == 3) {
            // should be done!
            if (breakpointIds.distinct.size == 1) {
              donePromise.success(())
            } else {
              donePromise.failure(new Exception(s"Breakpoint ID mismatch, ${breakpointIds.mkString(", ")}"))
            }
          } else getHost.resume()
        }
        observeAndRunScriptAsync(script, observer) { _ =>
          donePromise.future
        }
      }
    }

    "in a function that hasn't yet been executed" - {
      val script =
        """function fun(foo) {
          |  return foo;
          |}
          |debugger; // where we will set breakpoints
          |fun(42);
        """.stripMargin

      "should be possible" in {
        testSetBreakpoint(script, 2, None) { bp =>
          bp should be ('defined)
        }
      }

      "should be hit" in {
        val donePromise = Promise[Unit]()
        val observer = Observer.from[ScriptEvent] {
          case bp: HitBreakpoint =>
            val line = bp.stackFrames.head.location.lineNumber1Based
            if (line == 2) donePromise.success(())
            else if (line == 4) {
              setBreakpoint(bp, 2, None) match {
                case Some(_) => getHost.resume()
                case None => donePromise.failure(new Exception("Failed to set breakpoint at line 2"))
              }
            }
            else donePromise.failure(new Exception("Wrong line: " + line))
        }
        observeAndRunScriptAsync(script, observer) { _ =>
          donePromise.future
        }
      }
    }
  }

  private def breakAtLine(script: String, lineBase1: Int)(handler: (NashornScriptHost, HitBreakpoint) => Unit): Unit = {
    assert(!script.contains("debugger"), "No debugger statement in the script, please.")
    val realScript =
      s"""debugger;
         |$script
       """.stripMargin
    val breakpointLine = lineBase1 + 1 // +1 due to the debugger statement
    val donePromise = Promise[Unit]()
    val observer = Observer.from[ScriptEvent] {
      case bp: HitBreakpoint if bp.reason == BreakpointReason.Debugger =>
        setBreakpoint(bp, breakpointLine, None) match {
          case Some(x) =>
            getHost.resume()
          case None =>
            donePromise.failure(new Exception(s"Failed to set breakpoint at line $lineBase1"))
        }
      case bp: HitBreakpoint =>
        donePromise.complete(Try(handler(getHost, bp)))
    }
    observeAndRunScriptAsync(realScript, observer) { _ =>
      donePromise.future
    }
  }

  def setBreakpoint(bp: HitBreakpoint, line: Int, col: Option[Int]): Option[Breakpoint] = {
    val scriptId = bp.stackFrames.head.scriptId
    getHost.findScript(ScriptIdentity.fromId(scriptId)).map { s =>
      getHost.setBreakpoint(ScriptIdentity.fromURL(s.url), ScriptLocation(line, col), None)
    }
  }

  private def describeScope(host: ScriptHost, scope: Scope) = {
    scope.scopeType.getClass.getSimpleName.replace("$", "") + ":" + (scope.value match {
      case obj: ObjectNode =>
        host.getObjectProperties(obj.objectId, true, false).toMap.keys.mkString(",")
      case _ => ""
    })
  }

  case class MultiBreakpointsData(bp: HitBreakpoint, hitsSoFar: Int)
}
