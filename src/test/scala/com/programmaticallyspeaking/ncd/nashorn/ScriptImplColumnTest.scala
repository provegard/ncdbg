package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class ScriptImplColumnTest extends UnitTest with TableDrivenPropertyChecks {

  val cases = Table(
    ("desc", "code", "cols"),
    ("var + function", "var f = function() { return 42; };", Seq(1, 22)),
    ("nested functions", "function() { var f = function () { return 42; }; };", Seq(1, 14, 36)),
    ("indentation", "  return 5;", Seq(3)),
    ("function with arg", "function(arg) { return arg; };", Seq(1, 17)),
    ("var + named function", "var f = function foo() { return 42; };", Seq(1, 26)),
    ("var + function (ext syntax)", "var f = function sqrt(x) x * x;", Seq(1, 26)),
    ("space before arg list", "function () { return 42; };", Seq(1, 15)),
    ("tight function body", "function(){return 42; };", Seq(1, 12)),
    ("IIFE", "(function() { return 42; })();", Seq(1, 15)),
    ("empty line", "", Seq.empty),
    ("arrow function", "() => { return 42; }", Seq(1, 9)),
    ("arrow function (ext syntax)", "() => 42", Seq(1, 7))
  )
  //TODO: More!

  "ScriptImpl" - {
    "statementColumnsFor" - {
      forAll(cases) { (desc, line, cols) =>
        s"should handle: $desc" in {
          val actual = ScriptImpl.statementColumnsBase1For(line)
          actual should be (cols)
        }
      }
    }
  }
}
