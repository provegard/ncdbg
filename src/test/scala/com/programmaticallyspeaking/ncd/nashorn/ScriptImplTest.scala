package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ScriptImplTest extends UnitTest {

  def fromSource(path: String, source: String, id: String) =
    ScriptImpl.fromSource(ScriptURL.create(path), source, id)

  "ScriptImpl" - {
    "fromSource" - {
      "should do URL conversion" in {
        val script = fromSource("/some/script.js", "return null;", "a1")
        script.url.toString should be ("file:///some/script.js")
      }
    }

    "sourceURL" - {

      "is recognized" in {
        val source =
          """
            |var x = 5;
            |//# sourceURL=/path/to/script.coffee
          """.stripMargin
        val script = fromSource("/some/script.js", source, "a1")
        script.sourceUrl() should be (Some(ScriptURL.create("/path/to/script.coffee")))
      }

      "is resolved against the original URL" in {
        val source =
          """
            |var x = 5;
            |//# sourceURL=script.coffee
          """.stripMargin
        val script = fromSource("/some/script.js", source, "a1")
        script.sourceUrl() should be (Some(ScriptURL.create("/some/script.coffee")))
      }
    }

    "sourceMappingURL" - {

      "is recognized" in {
        val source =
          """
            |var x = 5;
            |//# sourceMappingURL=/path/to/script.js.map
          """.stripMargin
        val script = fromSource("/some/script.js", source, "a1")
        script.sourceMapUrl() should be (Some(ScriptURL.create("/path/to/script.js.map")))
      }

      "is resolved against the original URL" in {
        val source =
          """
            |var x = 5;
            |//# sourceMappingURL=script.js.map
          """.stripMargin
        val script = fromSource("/some/script.js", source, "a1")
        script.sourceMapUrl() should be (Some(ScriptURL.create("/some/script.js.map")))
      }
    }

    "sourceLine" - {
      val source = "var x = 5;"

      "returns an existing line" in {
        val script = fromSource("/some/script.js", source, "a1")
        script.sourceLine(1) should be (Some("var x = 5;"))
      }

      "returns None for a non-existent line" in {
        val script = fromSource("/some/script.js", source, "a1")
        script.sourceLine(2) should be ('empty)
      }
    }
  }
}
