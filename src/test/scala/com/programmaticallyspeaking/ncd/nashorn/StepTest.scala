package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{StepInto, StepOut, StepOver}

class StepTest extends StepTestFixture {

  "Stepping" - {
    "over" - {
      "passes a debugger statement" in {
        val script =
          """var x = 5;
            |debugger;
            |x = x + 1;
          """.stripMargin
        stepInScript(script, Seq(StepOver)) { location =>
          location.lineNumber1Based should be(3)
        }
      }

      "steps out of a function" in {
        val script =
          """var fun = function() {
            |  debugger;
            |};
            |fun();
            |fun.toString();
          """.stripMargin
        stepInScript(script, Seq(StepOver)) { location =>
          location.lineNumber1Based should be (5)
        }
      }

      "stops on a return after a call, staying in the callee" in {
        val script =
          """var foo = function() {
            |  debugger;
            |  return 42;
            |};
            |var bar = function() {
            |  return foo();
            |};
            |bar();
            |bar.toString();
          """.stripMargin
        stepInScript(script, Seq(StepOver, StepOver)) { location =>
          location.lineNumber1Based should be (6)
        }
      }

      "stops on a non-return after a call, staying in the callee" in {
        val script =
          """var foo = function() {
            |  debugger;
            |  return 42;
            |};
            |var bar = function() {
            |  foo();
            |};
            |bar();
            |bar.toString();
          """.stripMargin
        stepInScript(script, Seq(StepOver, StepOver)) { location =>
          location.lineNumber1Based should be (6)
        }
      }

      "passes a function call" in {
        val script =
          """var fun = function() {
            |  return 'testing';
            |};
            |debugger;
            |fun();
            |fun.toString();
          """.stripMargin
        stepInScript(script, Seq(StepOver, StepOver)) { location =>
          location.lineNumber1Based should be (6)
        }
      }

      "with a while loop" - {
        val script =
          """var z = 0;
            |debugger;
            |while (z < 2) {
            |  z++;
            |}
            |z++;
          """.stripMargin

        "jumps back to the loop condition after the body" in {
          stepInScript(script, Seq(StepOver, StepOver, StepOver)) { location =>
            location.lineNumber1Based should be (3)
          }
        }
      }

    }

    "out" - {
      "exits a function from a debugger statement" in {
        val script =
          """var fun = function() {
            |  debugger;
            |};
            |fun();
            |fun.toString();
          """.stripMargin
        stepInScript(script, Seq(StepOut)) { location =>
          location.lineNumber1Based should be (5)
        }
      }

      "exits a function from a debugger statement followed by a regular statement" in {
        val script =
          """var fun = function() {
            |  debugger;
            |  while (this.dummy) fun();
            |};
            |fun();
            |fun.toString();
          """.stripMargin
        stepInScript(script, Seq(StepOut)) { location =>
          location.lineNumber1Based should be (6)
        }
      }

      "exits a function from a non-debugger statement" in {
        val script =
          """var fun = function() {
            |  debugger;
            |  while (this.dummy) fun();
            |};
            |fun();
            |fun.toString();
          """.stripMargin
        stepInScript(script, Seq(StepOver, StepOut)) { location =>
          location.lineNumber1Based should be (6)
        }
      }

      "exits a function from a non-debugger statement followed by another regular statement" in {
        val script =
          """var fun = function() {
            |  debugger;
            |  while (this.dummy1) fun();
            |  while (this.dummy2) fun();
            |};
            |fun();
            |fun.toString();
          """.stripMargin
        stepInScript(script, Seq(StepOver, StepOut)) { location =>
          location.lineNumber1Based should be (7)
        }
      }
    }

    "into" - {
      "enters a function" in {
        val script =
          """var fun = function() {
            |  return 42;
            |};
            |fun(); // force compilation of 'fun'
            |debugger;
            |fun();
          """.stripMargin
        stepInScript(script, Seq(StepOver, StepInto)) { location =>
          location.lineNumber1Based should be (2)
        }
      }

      "enters a function after step over" in {
        val script =
          """var fun = function() {
            |  return 42;
            |};
            |fun(); // force compilation of 'fun'
            |debugger;
            |var f = fun;
            |f();
          """.stripMargin
        stepInScript(script, Seq(StepOver, StepOver, StepInto)) { location =>
          location.lineNumber1Based should be (2)
        }
      }

      "enters a function _after stepping out from another_" in {
        val script =
          """var a = function () { return 42; };
            |var b = function (x) { return x + 1 };
            |var fun = function() {
            |  debugger;
            |  return b(a());
            |};
            |fun();
          """.stripMargin
        stepInScript(script, Seq(StepOver, StepInto, StepOut, StepInto)) { location =>
          location.lineNumber1Based should be (2)
        }
      }

    }
  }
}
