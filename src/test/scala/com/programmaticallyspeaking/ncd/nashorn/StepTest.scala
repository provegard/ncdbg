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
        stepInScript(script, Seq(StepOver)) { bp =>
          bp.location.lineNumber1Based should be(3)
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
        stepInScript(script, Seq(StepOver)) { bp =>
          bp.location.lineNumber1Based should be (5)
        }
      }

      "steps out of two functions" in {
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
        stepInScript(script, Seq(StepOver, StepOver)) { bp =>
          bp.location.lineNumber1Based should be (9)
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
        stepInScript(script, Seq(StepOver, StepOver)) { bp =>
          bp.location.lineNumber1Based should be (6)
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
        stepInScript(script, Seq(StepOut)) { bp =>
          bp.location.lineNumber1Based should be (5)
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
        stepInScript(script, Seq(StepOut)) { bp =>
          bp.location.lineNumber1Based should be (6)
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
        stepInScript(script, Seq(StepOver, StepOut)) { bp =>
          bp.location.lineNumber1Based should be (6)
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
        stepInScript(script, Seq(StepOver, StepOut)) { bp =>
          bp.location.lineNumber1Based should be (7)
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
        stepInScript(script, Seq(StepOver, StepInto)) { bp =>
          bp.location.lineNumber1Based should be (2)
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
        stepInScript(script, Seq(StepOver, StepOver, StepInto)) { bp =>
          bp.location.lineNumber1Based should be (2)
        }
      }
    }
  }
}
