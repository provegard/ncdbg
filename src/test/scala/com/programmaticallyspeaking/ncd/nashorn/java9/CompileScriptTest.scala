package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.nashorn.{CompileScriptTestFixture, InvocationFailedException}

import scala.collection.mutable.ListBuffer

class CompileScriptTest extends CompileScriptTestFixture with RunningJava9 {

  "Script compilation for Java 9" - {

    "Compiling a script with a requested URL" - {
      val emitted = ListBuffer[ScriptAdded]()
      lazy val compiledScript = compileAndCollectScripts(Seq(("1+2+3", "file://url")), true, emitted.+=).head

      "returns a script with the correct contents" in {
        compiledScript.contents should include("1+2+3")
      }
    }

    "Compiling a script with persist=false" - {
      val emitted = ListBuffer[ScriptAdded]()
      lazy val compiledScript = compileAndCollectScripts(Seq(("1+2+5", "")), false, emitted.+=)

      "doesn't return the script" in {
        compiledScript should be ('empty)
      }
    }

    "Running a compiled script" - {
      lazy val result = compileAndRun("1+2+3", "")

      "gives the correct result" in {
        result should be (SimpleValue(6))
      }
    }

    "Compiling a script with an error" - {
      "results in an error" in {
        val ex = intercept[InvocationFailedException](compileAndCollectScripts(Seq(("'foo", "")), false))
        ex.getMessage should include ("Missing close quote")
      }
    }

  }
}
