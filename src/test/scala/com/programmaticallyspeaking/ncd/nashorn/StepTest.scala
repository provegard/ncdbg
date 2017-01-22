package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{StepInto, StepOut, StepOver}

class StepTest extends StepTestFixture {

  "Stepping" - {
    "works for regular step over" in {
      val script =
        """var x = 5;
          |debugger;
          |x = x + 1;
        """.stripMargin
      stepInScript(script, Seq(StepOver)) { bp =>
        bp.lineNumberBase1 should be (3)
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
      stepInScript(script, Seq(StepOut)) { bp =>
        bp.lineNumberBase1 should be (5)
      }
    }

    "steps into a function" in {
      val script =
        """var fun = function() {
          |  return 42;
          |};
          |fun(); // force compilation of 'fun'
          |debugger;
          |fun();
        """.stripMargin
      stepInScript(script, Seq(StepOver, StepInto)) { bp =>
        bp.lineNumberBase1 should be (2)
      }
    }

    "steps out of a function via step-over" in {
      val script =
        """var fun = function() {
          |  debugger;
          |};
          |fun();
          |fun.toString();
        """.stripMargin
      stepInScript(script, Seq(StepOver)) { bp =>
        bp.lineNumberBase1 should be (5)
      }
    }

    "steps over a function call" in {
      val script =
        """var fun = function() {
          |  return 'testing';
          |};
          |debugger;
          |fun();
          |fun.toString();
        """.stripMargin
      stepInScript(script, Seq(StepOver, StepOver)) { bp =>
        bp.lineNumberBase1 should be (6)
      }
    }
  }
}
