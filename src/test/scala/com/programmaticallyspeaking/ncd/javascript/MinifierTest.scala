package com.programmaticallyspeaking.ncd.javascript

import com.programmaticallyspeaking.ncd.testing.UnitTest

class MinifierTest extends UnitTest {

  "Minifier" - {
    "minifies code" in {
      val code =
        """
          |function fun(foo, bar, baz) {
          |  if (foo) {
          |    return bar + baz;
          |  } else {
          |    return bar * baz;
          |  }
          |}
        """.stripMargin
      val result = Minifier.minify(code)
      result should include ("c?a+b:a*b")
    }

    "doesn't transpile to ES5" in {
      val code =
        """
          |function *fun() {
          |  yield 42;
          |}
        """.stripMargin
      val result = Minifier.minify(code)
      result should include ("yield")
    }

    "doesn't introduce 'use strict'" in {
      val code =
        """
          |function fun() {
          |}
        """.stripMargin
      val result = Minifier.minify(code)
      result should not include ("use strict")
    }

  }
}
