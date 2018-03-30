package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, ObjectPropertyDescriptor, Undefined}
import com.programmaticallyspeaking.ncd.host._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

class EvaluateTest extends EvaluateTestFixture with TableDrivenPropertyChecks {

  private def testSuccess[A](tr: Try[A])(tester: (A) => Unit): Unit = tr match {
    case Success(value) => tester(value)
    case Failure(t) => fail(t)
  }

  val cases = Table(
    ("desc", "script", "expression", "result"),
    ("closure-captured value on the topmost stack frame",
      """var x = 21;
        |var fun = function () {
        |  debugger;
        |  x.toString(); // make sure x is captured
        |};
        |fun();
      """.stripMargin, "x+x", SimpleValue(42)),
    ("local non-primitive variable on the topmost stack frame",
      """var arr = [42];
        |debugger;
        |arr.toString(); // make sure arr isn't optimized away
      """.stripMargin, "arr.length", SimpleValue(1)),
    // TODO: If the variable used here is named 'x', then the test works in isolation but fails when run together
    // TODO: with the other test that uses 'x'. The error message is similar to the one in this bug report:
    // TODO: http://bugs.java.com/view_bug.do?bug_id=8056978
    ("local integer variable on the topmost stack frame",
      """var y = 21;
        |debugger;
        |y.toString(); // make sure x isn't optimized away
      """.stripMargin, "y+y", SimpleValue(42)),
    ("local floating-point variable on the topmost stack frame",
      """var y = 21.6;
        |debugger;
        |y.toString(); // make sure x isn't optimized away
      """.stripMargin, "y+y", SimpleValue(43.2d)),
    ("local bool variable on the topmost stack frame",
      """var y = true;
        |debugger;
        |y.toString(); // make sure x isn't optimized away
      """.stripMargin, "!y", SimpleValue(false)),
    ("closure-captured value when there also is a local",
      """var x = 21;
        |var fun = function () {
        |  var local = 22;
        |  debugger;
        |  x.toString(); // capture x
        |  local.toString(); // don't optimize away local
        |};
        |fun();
      """.stripMargin, "x", SimpleValue(21)),
    ("var statement on global level", "debugger;", "var x = 55;", SimpleValue(Undefined)),
    ("JavaBeans getter via property access",
      s"""var obj = createInstance('${classOf[ClassWithJavaBeans].getName}');
         |debugger;
         |obj.toString();
       """.stripMargin, "obj['fooBar']", SimpleValue("bar")),
    ("JavaBeans setter via property access",
      s"""var obj = createInstance('${classOf[ClassWithJavaBeans].getName}');
         |debugger;
         |obj.toString();
       """.stripMargin, "obj['fooBar']='qux'; obj['fooBar']", SimpleValue("qux"))
  )

  val varRemember = Table(
    ("desc", "script"),
    ("in a function with a local variable", "(function () { var freeVar = 0; debugger; freeVar.toString(); })();"),
    ("in a function without local variables", "(function () { debugger; })();"),
    ("in a function called on an object", "function fun() { debugger; } var obj = {x:fun}; obj.x();")
  )

  "Evaluating on a stack frame" - {
    def evaluate(script: String, expression: String)(handler: (ValueNode) => Unit): Unit = {
      evaluateInScript(script) { (host, stackframes) =>
        testSuccess(host.evaluateOnStackFrame(stackframes.head.id, expression, Map.empty))(handler)
      }
    }

    def evaluateError(script: String, expression: String)(handler: (ErrorValue) => Unit) = {
      evaluate(script, expression) {
        case e: ErrorValue => handler(e)
        case other => fail("Unexpected: " + other)
      }
    }

    forAll(cases) { (desc, script, expression, result) =>
      s"works for $desc" in {
        evaluate(script, expression) { value =>
          value should be(result)
        }
      }
    }

    // Failing test for #20
    forAll(varRemember) { (desc, script) =>
      s"remembers a var-defined variable $desc" in {
        evaluateInScript(script) { (host, stackframes) =>
          testSuccess(for {
            _ <- host.evaluateOnStackFrame(stackframes.head.id, "var zz = 21;", Map.empty)
            result <- host.evaluateOnStackFrame(stackframes.head.id, "zz+zz", Map.empty)
          } yield result) { r =>
            r should be (SimpleValue(42))
          }
        }
      }
    }

    "commits a change to a local var-defined variable" in {
      val script = "(function () { var xx = 5; debugger; debugger; return xx; })();"
      evaluateInScript(script)({ (host, stackframes) =>
        host.evaluateOnStackFrame(stackframes.head.id, "xx = 99;", Map.empty)
      }, { (host, stackframes) =>
        testSuccess(host.evaluateOnStackFrame(stackframes.head.id, "xx", Map.empty)) { r =>
          r should be (SimpleValue(99))
        }
      })
    }

    "ignores a debugger statement in the evaluated code" in {
      val script =
        """(function (x) {
          |  debugger;
          |  return x;
          |})(42)
        """.stripMargin
      evaluateInScript("debugger;") { (host, stackframes) =>
        val result = host.evaluateOnStackFrame(stackframes.head.id, script, Map.empty)
        result should be (Success(SimpleValue(42)))
      }
    }

    "detects a script added using 'load'" in {
      val foundIt = Promise[Unit]()

      val loadIt =
        """
          |load({
          |  script: "var x = 42 * 42;",
          |  name: "loaded.js"
          |});
        """.stripMargin

      def recordEvent(e: ScriptEvent): Unit = e match {
        case s: ScriptAdded if s.script.contents.contains("42 * 42") => foundIt.success(())
        case _ =>
      }
      evaluateInScript("debugger;", recordEvent) { (host, stackframes) =>
        host.evaluateOnStackFrame(stackframes.head.id, loadIt, Map.empty).get // get forces any failure
      }

      whenReady(foundIt.future) { _ => }
    }

    "and throwing a JS error" - {
      val script = "(function () { debugger; })();"
      val expression = "throw new TypeError('ugh');"

      "results in the correct exception data" in {
        evaluateError(script, expression) { err =>
          // Reset column number since it's affected by the "evaluated code marker"
          err.data.copy(stackIncludingMessage = None, columnNumberBase0 = 0) should be (ExceptionData(
            "TypeError", "ugh", 1, 0, "<eval>", None))
        }
      }

      "results in the stack trace data including message" in {
        evaluateError(script, expression) { err =>
          err.data.stackIncludingMessage match {
            case Some(stack) =>
              stack should fullyMatch regex ("(?s).*TypeError: ugh.*at <program> \\(<eval>:1\\).*")
            case None => fail("no stack")
          }
        }
      }
    }

    "marks a thrown Java exception as thrown" in {
      val script = "(function () { debugger; })();"
      val expression =
        """var Type = Java.type("java.lang.RuntimeException");
          |throw new Type("oops");
        """.stripMargin
      evaluateError(script, expression) { err =>
        err.isThrown should be (true)
      }
    }

    "marks an evaluated Java exception as not thrown" in {
      val script =
        """try {
          |  var Type = Java.type("java.lang.RuntimeException");
          |  throw new Type("oops");
          |} catch (e) {
          |  debugger;
          |}
        """.stripMargin
      evaluateError(script, "e") { err =>
        err.isThrown should be (false)
      }
    }

    "detects reference error inside function with artifical local scope" in {
      val script =
        """
          |function fun() {
          |  var obj = {};
          |  debugger;
          |  obj.toString();
          |}
          |fun();
        """.stripMargin
      evaluateInScript(script)({ (host, stackframes) =>
        host.evaluateOnStackFrame(stackframes.head.id, "unknown", Map.empty) match {
          case Success(vn: ErrorValue) =>
            vn.data.stackIncludingMessage.getOrElse("") should startWith ("""ReferenceError: "unknown" is not defined""")
          case Success(other) =>
            fail("Unexpected value: " + other)
          case Failure(t) =>
            fail("Error", t)
        }
      })
    }
  }
}

