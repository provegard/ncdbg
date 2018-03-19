package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ScriptIdentity
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.testing.IsolatedUnitTest

class ScriptsTest extends IsolatedUnitTest {

  "The Scripts repository" - {
    val scripts = new Scripts

    "when a script is suggested" - {
      val script = aScript("script.js", "return 42;", "a")
      scripts.suggest(script)

      "accepts it" in {
        scripts.scripts.map(_.id) should be (Seq("a"))
      }

      "allows lookup by ID" in {
        scripts.byId(ScriptIdentity.fromId("a")).map(_.id) should be (Some("a"))
      }

      "allows lookup by URL" in {
        scripts.byId(ScriptIdentity.fromURL("script.js")).map(_.id) should be (Some("a"))
      }
    }

    "when an anonymous script is suggested" - {
      val script = aScript("", "return 42;", "a")
      scripts.suggest(script)

      "accepts it" in {
        scripts.scripts.map(_.id) should be (Seq("a"))
      }

      "allows lookup by ID" in {
        scripts.byId(ScriptIdentity.fromId("a")).map(_.id) should be (Some("a"))
      }

      "doesn't allow lookup by URL" in {
        scripts.byId(ScriptIdentity.fromURL("")) should be (None)
      }
    }

    "when a script with the same contents as an existing one is added" - {
      val script1 = aScript("script.js", "return 42;", "a")
      val script2 = aScript("scriptRecompilation.js", "return 42;", "b")
      scripts.suggest(script1)
      val actual = scripts.suggest(script2)

      "returns the original one" in {
        actual shouldBe theSameInstanceAs(script1)
      }

      "allows lookup with the new URL (and returns the original script)" in {
        scripts.byId(ScriptIdentity.fromURL("scriptRecompilation.js")).map(_.id) should be (Some("a"))
      }

      "only returns the target script once in an enumeration" in {
        val ids = scripts.scripts.map(_.id)
        ids should be (Seq("a"))
      }
    }

    "when a script with different contents but same URL is added" - {
      val script1 = aScript("script.js", "return 42;", "a")
      val script2 = aScript("script.js", "return 52;", "b")
      scripts.suggest(script1)
      val actual = scripts.suggest(script2)

      "returns a script with a generated ID" in {
        actual.id should not be ("b")
      }

      "returns a script with a modified URL" in {
        actual.url.toString should fullyMatch regex("script_.*\\.js$")
      }

      "returns the old script for lookup with the URL" in {
        scripts.byId(ScriptIdentity.fromURL("script.js")).map(_.contents) should be (Some("return 42;"))
      }

      "returns the new script for lookup with the new URL" in {
        scripts.byId(ScriptIdentity.fromURL(actual.url)).map(_.contents) should be (Some("return 52;"))
      }

      "doesn't have a script with the suggested ID" in {
        scripts.byId(ScriptIdentity.fromId("b")) should be (None)
      }

      "has a script with the new ID" in {
        scripts.byId(ScriptIdentity.fromId(actual.id)).map(_.contents) should be (Some("return 52;"))
      }

      "doesn't enumerate the script with its suggested ID" in {
        val ids = scripts.scripts.map(_.id)
        ids should not contain ("b")
      }
    }

    "rejects suggestion of the same script twice" in {
      val script = aScript("script.js", "return 42;", "a")
      scripts.suggest(script)
      assertThrows[IllegalArgumentException](scripts.suggest(script))
    }
  }

  def aScript(url: String, source: String, id: String) =
    ScriptImpl.fromSource(ScriptURL.create(url), source, id)
}
