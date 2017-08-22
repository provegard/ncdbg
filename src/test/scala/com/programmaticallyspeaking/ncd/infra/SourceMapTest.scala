package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.testing.UnitTest

class SourceMapTest extends UnitTest {

  "SourceMap" - {

    "exposes sources" in {
      val json = """{"sources":["a.coffee","b.coffee"]}"""
      val sm = SourceMap.fromJson(json)
      sm.sources should be (Seq("a.coffee", "b.coffee"))
    }

    "ignores empty sources" in {
      val json = """{"sources":["","b.coffee"]}"""
      val sm = SourceMap.fromJson(json)
      sm.sources should be (Seq("b.coffee"))
    }
  }
}
