package com.programmaticallyspeaking.ncd.nashorn

import java.util.concurrent.LinkedBlockingQueue

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

class CompileScriptTest extends CompileScriptTestFixture {

  "Compiling a script with a requested URL" - {
    val emitted = ListBuffer[ScriptAdded]()
    lazy val compiledScript = compileAndCollectScripts(Seq(("1+2+3", "file://url")), true, emitted.+=).head

    "returns a script with the correct contents" in {
      compiledScript.contents should include ("1+2+3")
    }

    "uses the requested URL (with ScriptURL transformation) for the script" in {
      compiledScript.url.toString should be ("file:///url")
    }

    "emits the script as ScriptAdded" in {
      val compiledId = compiledScript.id
      emitted.map(_.script.id) should contain (compiledId)
    }
  }

  "Compiling a script with persist=false" - {
    val emitted = ListBuffer[ScriptAdded]()
    lazy val compiledScript = compileAndCollectScripts(Seq(("1+2+5", "")), false, emitted.+=)

    "doesn't return the script" in {
      compiledScript should be ('empty)
    }

    "doesn't emit the script as ScriptAdded" in {
      emitted.map(_.script.contents).filter(_.contains("1+2+5")) should be ('empty)
    }

    "and THEN compiling with persist=true returns the script" in {
      val res = compileAndCollectScripts(Seq(("1+2+5", "")), persist = true, preventRestart = true).headOption
      res should be ('defined)
    }
  }

  "Compiling a script with an error" - {
    "results in an error" in {
      val ex = intercept[InvocationFailedException](compileAndCollectScripts(Seq(("'foo", "")), false))
      ex.getMessage should include ("Missing close quote")
    }
  }

  "Compiling two anonymous scripts with empty requested URLs" - {

    lazy val compiledScripts = {
      val scripts = compileAndCollectScripts(Seq(("1+2+3", ""), ("4+5+6", "")))
      (scripts.head, scripts(1))
    }

    "allows lookup of script 1 by ID" in {
      val (s1, _) = compiledScripts
      getHost.findScript(IdBasedScriptIdentity(s1.id)).map(_.contents).getOrElse("") should include ("1+2+3")
    }

    "allows lookup of script 2 by ID" in {
      val (_, s2) = compiledScripts
      getHost.findScript(IdBasedScriptIdentity(s2.id)).map(_.contents).getOrElse("") should include ("4+5+6")
    }

    "uses the requested empty URL for a script" in {
      val (s1, _) = compiledScripts
      s1.url.toString should be ("")
    }

    "doesn't allow script lookup by empty URL" in {
      val (s1, _) = compiledScripts
      getHost.findScript(URLBasedScriptIdentity(s1.url.toString)) should be (None)
    }
  }

  "Compiling the same script twice" - {

    lazy val compiledScripts = {
      val scripts = compileAndCollectScripts(Seq(("1+2+3", ""), ("1+2+3", "")))
      (scripts.head, scripts(1))
    }

    "reuses the Script instance" in {
      val (s1, s2) = compiledScripts
      s2.id should be (s1.id)
    }
  }

  "Running a compiled script" - {

    lazy val result = compileAndRun("1+2+3", "")

    "gives the correct result" in {
      result should be (SimpleValue(6))
    }

    "works many times" ignore {
      for (i <- 1 to 20) {
        compileAndRun("1+2+3", "") should be (SimpleValue(6))
      }
    }
  }

  "Running a compiled script with a function" - {

    lazy val result = compileAndRun(
      """function f(a,b,c) {
        |  return a + b + c;
        |}
        |f(1, 2, 3)
      """.stripMargin, "")

    "gives the correct result" in {
      result should be (SimpleValue(6))
    }
  }

  "Running a compiled script with a debugger statement" - {

    lazy val result = compileAndRun(
      """var x = 5, y = 6;
        |debugger;
        |x + y
      """.stripMargin, "")

    "works since debugging is disabled while compiled scripts are run" in {
      result should be (SimpleValue(11))
    }
  }

  "Evaluating code after syntax-checking the same code" - {
    "inside a local scope" - {
      val runner =
        """(function (value) {
          |  debugger;
          |})(42);
        """.stripMargin
      val code = "value * 2"
      lazy val result = syntaxCheckThenEval(runner, code)

      "gives the expected result" in {
        result should be (Success(SimpleValue(84)))
      }
    }
//    //TODO: No reuse across stack frames
//    "inside a 'with' scope" - {
//      val runner =
//        """(function () {
//          |  var obj = { value: 43 };
//          |  with (obj) {
//          |    debugger;
//          |    return value;
//          |  }
//          |})();
//        """.stripMargin
//      val code = "value - 1"
//      lazy val result = syntaxCheckThenEval(runner, code)
//
//      "gives the expected result" in {
//        result should be (Success(SimpleValue(42)))
//      }
//    }

    "inside a closure scope" - {
      val runner =
        """(function () {
          |  var value = 44;
          |  function f() {
          |    debugger;
          |    return value;
          |  }
          |  f();
          |})();
        """.stripMargin
      val code = "value + 1"
      lazy val result = syntaxCheckThenEval(runner, code)

      "gives the expected result" in {
        result should be (Success(SimpleValue(45)))
      }
    }
  }

  "Running a compiled script with a reference error" - {
    val script =
      """function f() {
        |  return zz;
        |}
        |f()
      """.stripMargin

    "gives the same error result as if the code was evaluated" in {
      val runResult = compileAndRun(script, "")
      runResult match {
        case ev: ErrorValue =>
          ev.isThrown should be (true)
        case other =>
          fail("Unexpected: " + other)
      }
    }
  }
}

class CompileScriptTestFixture extends UnitTest with NashornScriptHostTestFixture with ScalaFutures with IntegrationPatience {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  type Tester = ScriptHost => Future[Unit]

  protected def runTest(collectScriptAdded: ScriptAdded => Unit = _ => {}, runnerCode: String = "debugger;", preventRestart: Boolean = false)(tester: Tester): Unit = {
    val donePromise = Promise[Unit]()
    val observer = Observer.from[ScriptEvent] {
      case _: HitBreakpoint =>
        val host = getHost
        donePromise.future.onComplete(_ => host.resume())
        donePromise.completeWith(tester(host))
      case s: ScriptAdded =>
        collectScriptAdded(s)
      case _ => // ignore
    }
    observeAndRunScriptAsync(runnerCode, observer, preventRestart = preventRestart)(_ => donePromise.future)
  }

  def compileAndRun(code: String, url: String): ValueNode = {
    var result: Try[ValueNode] = null
    runTest() { host =>
      host.compileScript(code, url, persist = true).map { s =>
        result = host.runCompiledScript(s.map(_.id).orNull)
      }
    }
    result.get
  }

  def syntaxCheckThenEval(runner: String, code: String): Try[ValueNode] = {
    var result: Try[ValueNode] = null
    runTest(_ => (), runner) { host =>
      host.compileScript(code, "", persist = false).map { _ =>
        result = host.evaluateOnStackFrame(StackFrame.TopId, code)
      }
    }
    result
  }

  def compileAndCollectScripts(codesAndUrls: Seq[(String, String)], persist: Boolean = true, collectEmitted: ScriptAdded => Unit = _ => {}, preventRestart: Boolean = false): Seq[Script] = {
    import scala.collection.JavaConverters._
    val scripts = new LinkedBlockingQueue[Script]()
    runTest(collectEmitted, preventRestart = preventRestart) { host =>
      var f = Future.successful(())
      codesAndUrls.foreach { tup =>
        val (code, url) = tup
        f = f.flatMap { _ =>
          host.compileScript(code, url, persist).map {
            case Some(s) => scripts.put(s)
            case None =>
          }
        }
      }
      f
    }
    scripts.asScala.toSeq
  }

}
