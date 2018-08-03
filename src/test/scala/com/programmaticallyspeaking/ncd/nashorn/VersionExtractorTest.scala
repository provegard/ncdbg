package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ScriptVersion
import com.programmaticallyspeaking.ncd.testing.UnitTest

class VersionExtractorTest extends UnitTest {
  "Version extraction" - {
    "Extracts from a script without compilation ID" in {
      val name = "jdk.nashorn.internal.scripts.Script$\\^eval\\_"
      VersionExtractor.extract(name) should be (ScriptVersion(0, true))
    }

    "Extracts from a script with compilation ID" in {
      val name = "jdk.nashorn.internal.scripts.Script$2$foobar"
      VersionExtractor.extract(name) should be (ScriptVersion(2, true))
    }

    "Extracts from a Recompilation script" in {
      val name = "jdk.nashorn.internal.scripts.Script$Recompilation$2$foobar"
      VersionExtractor.extract(name) should be (ScriptVersion(2, false))
    }
  }
}
