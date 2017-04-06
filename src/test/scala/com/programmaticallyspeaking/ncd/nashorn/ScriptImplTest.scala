package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.testing.UnitTest

class ScriptImplTest extends UnitTest {

  "ScriptImpl" - {
    "fromSource" - {
      "should do URL conversion" in {
        var script = ScriptImpl.fromSource("/some/script.js", "return null;", "a1")
        script.uri.toString should be ("file:///some/script.js")
      }
    }
  }
}
