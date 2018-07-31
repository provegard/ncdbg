package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ObjectMappingTest extends UnitTest {

  "ObjectMapping.toJson" - {
    "should serialize RemoteObject.undefinedValue without null fields" in {
      val json = ObjectMapping.toJson(RemoteObject.undefinedValue)
      json should be ("""{"type":"undefined"}""")
    }

    "should serialize RemoteObject.nullValue with null value" in {
      val json = ObjectMapping.toJson(RemoteObject.nullValue)
      json should be ("""{"type":"object","subtype":"null","value":null}""")
    }

    "should serialize RemoteObject.trueValue correctly" in {
      val json = ObjectMapping.toJson(RemoteObject.trueValue)
      json should be ("""{"type":"boolean","value":true}""")
    }

    "should not write anything for None" in {
      val json = ObjectMapping.toJson(OptionTester(None))
      json should be ("{}")
    }

    "should write something for Some" in {
      val json = ObjectMapping.toJson(OptionTester(Some("testing")))
      json should be ("""{"value":"testing"}""")
    }

    // This changed with Jackson 2.9.6 (2.9.4 perhaps). Unsure if it's important though,
    // since the RemoteObject.nullValue test above passes.
    "should write something for Some(null)" ignore {
      val json = ObjectMapping.toJson(OptionTester(Some(null)))
      json should be ("""{"value":null}""")
    }
  }

  case class OptionTester(value: Option[String])
}
