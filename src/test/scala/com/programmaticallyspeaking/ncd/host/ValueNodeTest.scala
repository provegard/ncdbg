package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ExceptionData
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ValueNodeTest extends UnitTest {

  "ErrorValue" - {
    val exData = ExceptionData("TypeError", "oops", 10, 0, "<eval>", None, None)
    def errValue(data: ExceptionData) =
      ErrorValue(data, isBasedOnThrowable = false, ObjectId("$err"))

    def valueFor(ev: ErrorValue, key: String): Option[ValueNode] =
      ev.entries.find(_._1 == key).map(_._2.resolve())

    "should expose a 'message' entry" in {
      val result = valueFor(errValue(exData), "message")
      result should be (Some(SimpleValue("oops")))
    }

    "should expose a 'name' entry" in {
      val result = valueFor(errValue(exData), "name")
      result should be (Some(SimpleValue("TypeError")))
    }

    "should not expose a 'stack' entry if there is no stack" in {
      val result = valueFor(errValue(exData), "stack")
      result should be (None)
    }

    "should not expose a 'javaStack' entry if there is no stack" in {
      val result = valueFor(errValue(exData), "javaStack")
      result should be (None)
    }

    "should expose a 'stack' entry if there is a stack" in {
      val exData = ExceptionData("TypeError", "oops", 10, 0, "<eval>", Some("the stack"), None)
      val result = valueFor(errValue(exData), "stack")
      result should be (Some(SimpleValue("the stack")))
    }

    "should expose a 'javaStack' entry if there is a Java stack" in {
      val exData = ExceptionData("TypeError", "oops", 10, 0, "<eval>", None, Some("Java stack"))
      val result = valueFor(errValue(exData), "javaStack")
      result should be (Some(SimpleValue("Java stack")))
    }
  }

  "FunctionNode" - {
    "should expose name among the entries" in {
      val fun = FunctionNode("aFun", "", Map.empty, ObjectId("x"))
      fun.entries.find(_._1 == "name").map(_._2.resolve()) should be (Some(SimpleValue("aFun")))
    }
  }
}
