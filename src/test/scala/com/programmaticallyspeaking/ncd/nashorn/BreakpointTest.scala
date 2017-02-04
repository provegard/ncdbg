package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{ObjectNode, Scope, ScriptHost}
import org.scalactic.Equality
import org.scalatest.prop.TableDrivenPropertyChecks

class BreakpointTest extends BreakpointTestFixture with TableDrivenPropertyChecks {

  val scopeTests = Table(
    ("desc", "script", "expectation"),
    ("on global level", "debugger;", Seq("Global:.*")),
    ("on global level with variable",
      """var globalVar = 0;
        |debugger;
        |globalVar.toString()
      """.stripMargin, Seq("Global:.*globalVar.*")),
    ("inside a function with a local variable",
      """(function () {
        |  var x = 0;
        |  debugger;
        |  x.toString();
        |})();
      """.stripMargin, Seq("Local:x", "Global:.*")),
    // TODO: Figure out how to see the 'foo' variable in the 'with' block.
    ("inside a with block inside a function",
      """(function () {
        |  var x = 0;
        |  var obj = { foo: 42 };
        |  with (obj) {
        |    debugger;
        |    x.toString(); // capture something from the surrounding scope
        |  }
        |})();
      """.stripMargin, Seq("Local:obj", "With:", "Closure:x", "Global:.*")),
    ("inside a function inside a function",
      """(function () {
        |  var x = 0;
        |  (function () {
        |    var y = 0;
        |    debugger;
        |    y.toString();
        |    x.toString(); // capture something from the parent function
        |  })();
        |})();
      """.stripMargin, Seq("Local:y", "Closure:x", "Global:.*")),
    ("inside a function inside a 'with' block inside a function",
      """(function () {
        |  var x = 0;
        |  var obj = { foo: 42 };
        |  with (obj) {
        |    (function () {
        |      var y = 0;
        |      debugger;
        |      y.toString();
        |      x.toString(); // capture something from the parent function
        |    })();
        |  }
        |})();
      """.stripMargin, Seq("Local:y", "With:", "Closure:x", "Global:.*")),
    ("inside a function where this != global",
      """(function () {
        |  var x = 0;
        |  debugger;
        |  x.toString();
        |}).call({});
      """.stripMargin, Seq("Local:x", "Global:.*"))
  )

  "when a breakpoint is hit" - {
    "the scopes should be sorted out" - {
      implicit val regexpEq = new Equality[Seq[String]] {
        override def areEqual(a: Seq[String], b: Any): Boolean = b match {
          case regexpes: Seq[String] =>
            a.size == regexpes.size && a.zip(regexpes).forall { case (str, regexp) =>
              regexp.r.pattern.matcher(str).matches
            }

          case _ => false
        }
      }
      forAll(scopeTests) { (desc, script, expectationRegExps) =>
        desc in {
          waitForBreakpoint(script) { (host, breakpoint) =>
            breakpoint.stackFrames.headOption match {
              case Some(st) =>

                st.scopeChain.map(s => describeScope(host, s)) should equal (expectationRegExps)

              case None => fail("no stack frames!")
            }
          }
        }
      }
    }
  }

  private def describeScope(host: ScriptHost, scope: Scope) = {
    scope.scopeType.getClass.getSimpleName.replace("$", "") + ":" + (scope.value match {
      case obj: ObjectNode =>
        host.getObjectProperties(obj.objectId, true, false).keys.mkString(",")
      case _ => ""
    })
  }

//  "bbb" - {
//    "fooo" in {
//      // this/scope = Global
////      val script = "debugger;"
//
//      // 1: this = Global
//      // 2: this/scope = Global
////      val script =
////        """(function () {
////          |debugger;
////          |})();
////        """.stripMargin
//      // this = Global
//      // scope = JO1P0
////      val script =
////        """try {
////          |  throw new Error("ex");
////          |} catch (e) {
////          |  debugger;
////          |}
////        """.stripMargin
//
//      // this = Global
//      // scope = WithObject
////      val script =
////        """var obj = { "foo": 42 };
////          |with (obj) {
////          |  debugger;
////          |}
////        """.stripMargin
//
//      val script =
//        """(function () {
//          |var foo = 42;
//          |debugger;
//          |foo.toString();
//          |})();
//        """.stripMargin
//
//      waitForBreakpoint(script) { (host, breakpoint) =>
//        breakpoint.stackFrames.foreach { st =>
//          println(st.scopeObj)
//          println(st.thisObj)
//          println(st.locals.extraEntries.keys.mkString(", "))
//        }
//      }
//    }
//  }
}
