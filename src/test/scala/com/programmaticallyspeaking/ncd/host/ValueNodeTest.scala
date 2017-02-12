package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.testing.UnitTest

class ValueNodeTest extends UnitTest {

  "ValueNode" - {
    "asString" - {
      "should return a string for a string value" in {
        val vn = SimpleValue("testing")
        vn.asString should be("testing")
      }

      "should return null for a non-string value" in {
        val vn = SimpleValue(42)
        vn.asString should be(null)
      }
    }

    "asBool" - {
      "should return a bool for a boolean value" in {
        val vn = SimpleValue(true)
        vn.asBool(false) should be(true)
      }

      "should return the default for a non-boolean value" in {
        val vn = SimpleValue(42)
        vn.asBool(false) should be(false)
      }
    }

    "asNumber" - {
      "should return a number for a integer value" in {
        val vn = SimpleValue(42)
        vn.asNumber(0) should be(42)
      }

      "should return a number for a double value" in {
        val vn = SimpleValue(42.0)
        vn.asNumber(0) should be(42.0)
      }

      "should return the default for a non-integer value" in {
        val vn = SimpleValue("testing")
        vn.asNumber(0) should be(0)
      }
    }
  }
}
