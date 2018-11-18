package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.testing.UnitTest

class NameConvertTest extends UnitTest {
  "sourceNameToUrl" - {
    "with multiple <eval> tags" - {
      val typeName = "jdk.nashorn.internal.scripts.Script$187$\\^eval\\_#138\\!9\\^eval\\_"
      val sourceName = "<eval>#138:9<eval>"

      "creates a URL" in {
        val url = NameConvert.sourceNameToUrl(typeName, sourceName)
        url should be("eval:/Script187/eval/#138/!9")
      }

      "creates a URL parseable by ScriptURL" in {
        val url = NameConvert.sourceNameToUrl(typeName, sourceName)
        val su = ScriptURL.create(url)

        su.toString should include ("Script187")
      }
    }
  }
}
