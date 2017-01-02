package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.testing.UnitTest

class ObjectIdTest extends UnitTest {

  "ObjectId" - {
    "should have a JSON toString representation" in {
      val id = ObjectId("x")
      id.toString should be ("""{"id":"x"}""")
    }

    "should be creatable from its string representation" in {
      val id = ObjectId("x")
      val str = id.toString
      ObjectId.fromString(str) should be (id)
    }
  }
}
