package com.programmaticallyspeaking.ncd.transpile

import com.programmaticallyspeaking.ncd.testing.UnitTest
import jdk.nashorn.api.scripting.NashornScriptEngineFactory

trait ES5TranspilerTest extends UnitTest {
  def newTranspiler: ES5Transpiler
  def description: String

  val transpiler = newTranspiler

  def newEngine = new NashornScriptEngineFactory().getScriptEngine("--language=es5")

  description - {
    "can transpile multiple times" in {
      transpiler.transpile("function *fun() { yield 42; }")
      val output = transpiler.transpile("function *fun() { yield 943; }")

      output should include ("943")
    }

    "generates a self-contained script for a generator function" in {
      val src = transpiler.transpile("(function () { var f = function *() { yield 'it works'; }; return f().next().value; })()")
      newEngine.eval(src).toString should be ("it works")
    }

    "can use the transpiled code as an expression" in {
      val src = transpiler.transpile("(function () { var f = function *() { yield 'it works'; }; return f().next().value; })()")
      newEngine.eval(s"($src)").toString should be ("it works")
    }

    "handles for..of with a generator" in {
      val script =
        """
          |(function() {
          |  function *numbers() { yield 21; yield 22; }
          |  var result = 0;
          |  for (var i of numbers())
          |    result += i;
          |  return result;
          |})()
        """.stripMargin
      var output = transpiler.transpile(script)
      newEngine.eval(output) should be (43.0)
    }

    "doesn't pollute the global object with polyfills and stuff" in {
      val engine = newEngine
      val src = transpiler.transpile("(function () { var f = function *() { yield 'it works'; }; return f().next().value; })()")
      engine.eval(src)
      val sym = engine.eval("this.Symbol")
      sym should be (null)
    }

    "is fast (after warmup)" ignore {
      import scala.concurrent.duration._
      // warmup
      transpiler.transpile("function *fun() { yield 42; }")

      val script =
        """
          |(function packRanges(fromIndex, toIndex, bucketThreshold, sparseIterationThreshold, getOwnPropertyNamesThreshold) {
          |	var ownPropertyNames = null;
          |	var consecutiveRange = (toIndex - fromIndex >= sparseIterationThreshold) && ArrayBuffer.isView(this);
          |	var skipGetOwnPropertyNames = consecutiveRange && (toIndex - fromIndex >= getOwnPropertyNamesThreshold);
          |	function  * arrayIndexes(object) {
          |		if (toIndex - fromIndex < sparseIterationThreshold) {
          |			for (var i = fromIndex; i <= toIndex; ++i) {
          |				if (i in object)
          |					yield i;
          |			}
          |		} else {
          |			ownPropertyNames = ownPropertyNames || Object.getOwnPropertyNames(object);
          |			for (var i = 0; i < ownPropertyNames.length; ++i) {
          |				var name = ownPropertyNames[i];
          |				var index = name >>> 0;
          |				if (('' + index) === name && fromIndex <= index && index <= toIndex)
          |					yield index;
          |			}
          |		}
          |	}
          |	var count = 0;
          |	if (consecutiveRange) {
          |		count = toIndex - fromIndex + 1;
          |	} else {
          |		for (var i of arrayIndexes(this))
          |			++count;
          |	}
          |	var bucketSize = count;
          |	if (count <= bucketThreshold)
          |		bucketSize = count;
          |	else
          |		bucketSize = Math.pow(bucketThreshold, Math.ceil(Math.log(count) / Math.log(bucketThreshold)) - 1);
          |	var ranges = [];
          |	if (consecutiveRange) {
          |		for (var i = fromIndex; i <= toIndex; i += bucketSize) {
          |			var groupStart = i;
          |			var groupEnd = groupStart + bucketSize - 1;
          |			if (groupEnd > toIndex)
          |				groupEnd = toIndex;
          |			ranges.push([groupStart, groupEnd, groupEnd - groupStart + 1]);
          |		}
          |	} else {
          |		count = 0;
          |		var groupStart = -1;
          |		var groupEnd = 0;
          |		for (var i of arrayIndexes(this)) {
          |			if (groupStart === -1)
          |				groupStart = i;
          |			groupEnd = i;
          |			if (++count === bucketSize) {
          |				ranges.push([groupStart, groupEnd, count]);
          |				count = 0;
          |				groupStart = -1;
          |			}
          |		}
          |		if (count > 0)
          |			ranges.push([groupStart, groupEnd, count]);
          |	}
          |	return {
          |		ranges: ranges,
          |		skipGetOwnPropertyNames: skipGetOwnPropertyNames
          |	};
          |}).apply(__obj_1, [0, 999, 100, 250000, 500000])
          |
        """.stripMargin

      val COUNT = 100
      val before = System.nanoTime()
      for (i <- 1 to COUNT) {
        transpiler.transpile(script)
      }
      val elapsed = (System.nanoTime() - before).nanos
      val perSec = COUNT / (elapsed.toMillis / 1000.0)
      println("per second = " + perSec)
    }
  }
}

class ClosureBasedTranspilerTest extends ES5TranspilerTest {
  override def newTranspiler: ES5Transpiler = new ClosureBasedES5Transpiler

  override def description: String = "Closure-based ES5 transpiler"
}
