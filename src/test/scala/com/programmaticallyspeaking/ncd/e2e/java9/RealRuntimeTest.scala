package com.programmaticallyspeaking.ncd.e2e.java9

import com.programmaticallyspeaking.ncd.chrome.domains.{Runtime => RuntimeD}
import com.programmaticallyspeaking.ncd.e2e.RealRuntimeTestFixture
import com.programmaticallyspeaking.ncd.nashorn.java9.RunningJava9
import org.scalatest.prop.TableDrivenPropertyChecks

class RealRuntimeTest extends RealRuntimeTestFixture with TableDrivenPropertyChecks with RunningJava9 {

  private def waitForDebugger(testers: Tester*) = {
    runScript("debugger;")(testers: _*)
  }

  private def compileWithError(code: String)(f: RuntimeD.CompileScriptResult => Unit) = {
    waitForDebugger(_ => {
      val result = sendRequest(RuntimeD.compileScript(code, "", false, None))
      result match {
        case r: RuntimeD.CompileScriptResult =>
          f(r)
        case other => fail("unexpected: " + other)
      }
    })
  }

  "Runtime (Java 9)" - {
    "compileScript" - {

      "reports a _translated_ error if the script is an incomplete template literal (with persist=false)" in {
        compileWithError("`foo") { r =>
          val Some(desc) = r.exceptionDetails.flatMap(_.exception).flatMap(_.description)
          desc should startWith ("SyntaxError: Unterminated template literal")
        }
      }

      "doesn't translate a non-template literal related quite error" in {
        compileWithError("'foo") { r =>
          val desc = r.exceptionDetails.flatMap(_.exception).flatMap(_.description)
          desc should be (None)
        }
      }
    }
  }
}
