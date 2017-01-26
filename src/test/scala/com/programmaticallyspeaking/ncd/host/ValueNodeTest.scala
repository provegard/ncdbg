package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.ExceptionData
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ValueNodeTest extends UnitTest {

  "ErrorValue" - {
    val exData = ExceptionData("TypeError", "oops", 10, 0, "<eval>", None, None)
    def errValue(data: ExceptionData) =
      ErrorValue(data, isBasedOnThrowable = false, ObjectId("$err"))

    def valueFor(ev: ErrorValue, key: String): Option[ValueNode] =
      ev.extraEntries.find(_._1 == key).map(_._2.resolve())

    "should not expose a 'javaStack' entry if there is no stack" in {
      val result = valueFor(errValue(exData), "__javastack__")
      result should be (None)
    }

    "should expose a 'javaStack' entry if there is a Java stack" in {
      val exData = ExceptionData("TypeError", "oops", 10, 0, "<eval>", None, Some("Java stack"))
      val result = valueFor(errValue(exData), "javaStack")
      result should be (Some(SimpleValue("Java stack")))
    }
  }
}
