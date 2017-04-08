package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.testing.UnitTest

class SourceMapTest extends UnitTest {

  "SourceMap" - {

    val json = """{"sources":["a.coffee","b.coffee"]}"""

    "exposes sources" in {
      val sm = SourceMap.fromJson(json)
      sm.sources should be (Seq("a.coffee", "b.coffee"))
    }
  }
}
