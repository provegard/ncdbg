package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ExceptionData
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ValueNodeTest extends UnitTest {

  "ErrorValue" - {
    val exData = ExceptionData("TypeError", "oops", 10, 0, "<eval>", None)

    def valueFor(ev: ErrorValue, key: String): Option[ValueNode] =
      ev.entries.find(_._1 == key).map(_._2.resolve())

    "should expose a 'message' entry" in {
      val result = valueFor(ErrorValue(exData, ObjectId("$err")), "message")
      result should be (Some(SimpleValue("oops")))
    }

    "should expose a 'name' entry" in {
      val result = valueFor(ErrorValue(exData, ObjectId("$err")), "name")
      result should be (Some(SimpleValue("TypeError")))
    }

    "should not expose a 'stack' entry if there is no stack" in {
      val result = valueFor(ErrorValue(exData, ObjectId("$err")), "stack")
      result should be (None)
    }

    "should expose a 'stack' entry if there is a stack" in {
      val exData = ExceptionData("TypeError", "oops", 10, 0, "<eval>", Some("the stack"))
      val result = valueFor(ErrorValue(exData, ObjectId("$err")), "stack")
      result should be (Some(SimpleValue("the stack")))
    }
  }
}
