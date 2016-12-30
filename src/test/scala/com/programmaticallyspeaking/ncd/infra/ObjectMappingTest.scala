package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ObjectMappingTest extends UnitTest {

  "ObjectMapping.toJson" - {
    "should serialize RemoteObject.undefinedValue without null fields" in {
      val json = ObjectMapping.toJson(RemoteObject.undefinedValue)
      json should be ("""{"type":"undefined"}""")
    }

    "should serialize RemoteObject.nullValue without null value" in {
      val json = ObjectMapping.toJson(RemoteObject.nullValue)
      json should be ("""{"type":"object","subtype":"null","value":null}""")
    }

    "should serialize RemoteObject.trueValue correctly" in {
      val json = ObjectMapping.toJson(RemoteObject.trueValue)
      json should be ("""{"type":"boolean","value":true}""")
    }
  }
}
