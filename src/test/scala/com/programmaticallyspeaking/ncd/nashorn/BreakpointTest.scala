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
      """.stripMargin, Seq("Local:x", "Global:.*"))
  )

  val exceptionTests = Table(
    ("desc", "thrownValue"),
    ("Error", "new Error('oops')"),
    ("value", "42")
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
        breakpoint.stackFrames.headOption.flatMap(_.breakpoint.location.columnNumber1Based) should be (Some(5))
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
            host.getObjectProperties(scopeObj.objectId, true, false).get("arg") match {
              case Some(propDesc) =>
                propDesc.isWritable should be (true)
              case None => fail("No property named 'arg'")
            }
          }
        }

        "should publish a variable (argument) as a data property with a value" in {
          evaluateLocalScopeObject(script) { (host, scopeObj) =>
            host.getObjectProperties(scopeObj.objectId, true, false).get("arg") match {
              case Some(propDesc) =>
                propDesc.value should be (Some(SimpleValue("test")))
              case None => fail("No property named 'arg'")
            }
          }
        }

        "should not leak the anonymous 'obj' object when getting all properties (not only own)" in {
          evaluateLocalScopeObject(script) { (host, scopeObj) =>
            host.getObjectProperties(scopeObj.objectId, false, false).keys should not contain ("obj")
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
            host.getObjectProperties(scopeObj.objectId, false, false).keys should contain ("sub")
          }
        }

        "should not include closure variables in the local scope, even if not-own properties are requested, because that is how Chrome works, it seems..." in {
          evaluateLocalScopeObject(script) { (host, scopeObj) =>
            host.getObjectProperties(scopeObj.objectId, false, false).keys should not contain ("number")
          }
        }
      }
    }
  }

  "when an exception is thrown" - {
    forAll(exceptionTests) { (desc, value) =>
      s"a caught $desc should be detected" in {
        val script =
          s"""try {
             |  throw $value; // row 2
             |} catch (e) {
             |  if (false) debugger; // waitForBreakpoint requires it
             |}
           """.stripMargin

        waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.Caught)) { (_, breakpoint) =>
          breakpoint.stackFrames.headOption.map(_.breakpoint.location.lineNumber1Based) should be(Some(2))
        }
      }
    }

    "a caught (by ScriptExecutor) non-Error value should be detected" in {
      val script =
        """throw 42; // row 1
          |if (false) debugger; // waitForBreakpoint requires it
          """.stripMargin

      waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.Caught)) { (_, breakpoint) =>
        breakpoint.stackFrames.headOption.map(_.breakpoint.location.lineNumber1Based) should be(Some(1))
      }
    }
  }

  "given a script with multiple breakable locations on the same line" - {
    val script =
      """function fun() {
        |  var foo = function() { return 42; } // multiple breakable here
        |  return foo;
        |}
        |fun()();  // ensure compilation of fun and foo
        |debugger; // where we will set breakpoints
        |fun()();  // two calls, should hit two breakpoints...
      """.stripMargin

    def createObserver(handler: ((MultiBreakpointsData) => Unit)): Observer[ScriptEvent] = {
      var breakpointsHitSoFar = 0
      val observer = Observer.from[ScriptEvent] {
        case bp: HitBreakpoint =>
          breakpointsHitSoFar += 1
          handler(MultiBreakpointsData(bp, breakpointsHitSoFar))
        case _ => // ignore
      }
      observer
    }

    def testSetBreakpoint(line: Int, col: Option[Int])(handler: (Option[Breakpoint]) => Unit): Unit = {
      val donePromise = Promise[Unit]()
      val observer = createObserver { data =>
        val scriptId = data.bp.stackFrames.head.breakpoint.scriptId
        val maybeBreakpoint = for {
          s <- getHost.scriptById(scriptId)
          bp <- getHost.setBreakpoint(s.url.toString, ScriptLocation(line, col), None)
        } yield bp

        donePromise.complete(Try(handler(maybeBreakpoint)))
        maybeBreakpoint.foreach(b => getHost.removeBreakpointById(b.breakpointId))
      }
      observeAndRunScriptAsync(script, observer) { _ =>
        donePromise.future
      }
    }

    "setting a breakpoint with no column returns all locations" in {
      testSetBreakpoint(2, None) { bp =>
        val columnNumbers = bp.map(_.locations.flatMap(_.columnNumber1Based))
        columnNumbers should be (Some(Seq(3, 26)))
      }
    }

    "setting a breakpoint with a column returns a single location" in {
      testSetBreakpoint(2, Some(3)) { bp =>
        val columnNumbers = bp.map(_.locations.flatMap(_.columnNumber1Based))
        columnNumbers should be (Some(Seq(3)))
      }
    }

    "setting a breakpoint with an incorrect column returns no breakpoint" in {
      testSetBreakpoint(2, Some(7)) { bp =>
        bp should be ('empty)
      }
    }

    "a breakpoint set for all locations on that line will get hit twice" in {
      val donePromise = Promise[Unit]()
      val breakpointIds = ListBuffer[String]()
      val observer = createObserver { data =>
        val scriptId = data.bp.stackFrames.head.breakpoint.scriptId
        val breakpointId = data.bp.stackFrames.head.breakpoint.breakpointId

        if (data.hitsSoFar == 1) {
          // debugger
          (for {
            s <- getHost.scriptById(scriptId)
            bp <- getHost.setBreakpoint(s.url.toString, ScriptLocation(2, None), None)
          } yield bp) match {
            case Some(bps) => breakpointIds += bps.breakpointId
            case None => donePromise.failure(new Exception("No script or breakpoint"))
          }
        } else breakpointIds += breakpointId

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

  private def describeScope(host: ScriptHost, scope: Scope) = {
    scope.scopeType.getClass.getSimpleName.replace("$", "") + ":" + (scope.value match {
      case obj: ObjectNode =>
        host.getObjectProperties(obj.objectId, true, false).keys.mkString(",")
      case _ => ""
    })
  }

  case class MultiBreakpointsData(bp: HitBreakpoint, hitsSoFar: Int)
}
