package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.host.{StepInto, StepOver}
import com.programmaticallyspeaking.ncd.nashorn.StepTestFixture

class StepTest extends StepTestFixture with RunningJava9 {

  "Stepping for Java 9" - {
    "into" - {
      "enters a function" in {
        val script =
          """var fun = function() {
            |  return 42;
            |};
            |var bar = function () {
            |  debugger;
            |  return fun();
            |};
            |bar();
          """.stripMargin
        // Java 9, due to module loading, generates extra calls to classes in the script package, e.g.:
        // - jdk.nashorn.internal.scripts.JD
        // - jdk.nashorn.internal.scripts.ModuleGraphManipulator
        // See: http://www.oracle.com/technetwork/java/jvmls2016-haupt-nashorn-3125551.pdf
        stepInScript(script, Seq(StepOver, StepInto)) { location =>
          location.lineNumber1Based should be (2)
        }
      }
    }
  }
}
