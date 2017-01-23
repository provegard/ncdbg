package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.SimpleValue

import scala.util.{Failure, Success, Try}

class EvaluateTest extends EvaluateTestFixture {

  private def testSuccess[A](tr: Try[A])(tester: (A) => Unit): Unit = tr match {
    case Success(value) => tester(value)
    case Failure(t) => fail(t)
  }

  "Evaluating on a stack frame" - {
    "works for a closure-captured value on the topmost stack frame" in {
      evaluateInScript(
        """var x = 21;
          |var fun = function () {
          |  debugger;
          |  x.toString(); // make sure x is captured
          |};
          |fun();
        """.stripMargin) { (host, stackframes) =>

        testSuccess(host.evaluateOnStackFrame(stackframes.head.id, "x+x", Map.empty)) { value =>
          value should be (SimpleValue(42))
        }
      }
    }

    "works for a local non-primitive variable on the topmost stack frame" in {
      evaluateInScript(
        """var arr = [42];
          |debugger;
          |arr.toString(); // make sure arr isn't optimized away
        """.stripMargin) { (host, stackframes) =>

        testSuccess(host.evaluateOnStackFrame(stackframes.head.id, "arr.length", Map.empty)) { value =>
          value should be (SimpleValue(1))
        }
      }
    }

    // TODO: If the variable used here is named 'x', then the test works in isolation but fails when run together
    // TODO: with the other test that uses 'x'. The error message is similar to the one in this bug report:
    // TODO: http://bugs.java.com/view_bug.do?bug_id=8056978
    "works for a local integer variable on the topmost stack frame" in {
      evaluateInScript(
        """var y = 21;
          |debugger;
          |y.toString(); // make sure x isn't optimized away
        """.stripMargin) { (host, stackframes) =>

        testSuccess(host.evaluateOnStackFrame(stackframes.head.id, "y+y", Map.empty)) { value =>
          value should be (SimpleValue(42))
        }
      }
    }

  }
}
