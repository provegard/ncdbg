package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.ExceptionData
import com.programmaticallyspeaking.ncd.host.{ErrorValue, SimpleValue, ValueNode}
import org.scalatest.prop.TableDrivenPropertyChecks

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
      """.stripMargin, "x", SimpleValue(21))
  )

  "Evaluating on a stack frame" - {
    def evaluate(script: String, expression: String)(handler: (ValueNode) => Unit): Unit = {
      evaluateInScript(script) { (host, stackframes) =>
        testSuccess(host.evaluateOnStackFrame(stackframes.head.id, expression, Map.empty))(handler)
      }
    }

    forAll(cases) { (desc, script, expression, result) =>
      s"works for $desc" in {
        evaluate(script, expression) { value =>
          value should be(result)
        }
      }
    }

    "of a thrown JS error" - {
      val script = "(function () { debugger; })();"
      val expression = "throw new TypeError('ugh');"

      def evaluateError(handler: (ExceptionData) => Unit) = {
        evaluate(script, expression) {
          case ErrorValue(data, true, _) => handler(data)
          case other => fail("Unexpected: " + other)
        }
      }

      "results in the correct exception data" in {
        evaluateError { data =>
          data.copy(stackIncludingMessage = None) should be (ExceptionData(
            "jdk.nashorn.internal.runtime.ECMAException", "TypeError: ugh", 1, 0, "<eval>", None))
        }
      }

      "results in the stack trace data including message" in {
        evaluateError { data =>
          data.stackIncludingMessage match {
            case Some(stack) =>
              stack should fullyMatch regex ("(?s).*TypeError: ugh.*at <program> \\(<eval>:1\\).*")
            case None => fail("no stack")
          }
        }
      }
    }
  }
}
