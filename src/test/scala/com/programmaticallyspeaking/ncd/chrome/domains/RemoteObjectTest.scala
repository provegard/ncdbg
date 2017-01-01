package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.testing.UnitTest

class RemoteObjectTest extends UnitTest {
  import Runtime._

  "RemoteObject" - {
    "forNumber" - {
      "should handle a normal number" in {
        RemoteObject.forNumber(42.0d) should be (RemoteObject("number", null, null, "42.0", 42.0d, null, null))
      }

      "should handle NaN" in {
        RemoteObject.forNumber(Double.NaN) should be (RemoteObject("number", null, null, "NaN", null, "NaN", null))
      }

      "should handle positive Infinity" in {
        RemoteObject.forNumber(Double.PositiveInfinity) should be (RemoteObject("number", null, null, "Infinity", null, "Infinity", null))
      }

      "should handle negative Infinity" in {
        RemoteObject.forNumber(Double.NegativeInfinity) should be (RemoteObject("number", null, null, "-Infinity", null, "-Infinity", null))
      }

      "should handle 0 (integer)" in {
        RemoteObject.forNumber(0) should be (RemoteObject("number", null, null, "0", 0, null, null))
      }

      "should handle a long number" in {
        RemoteObject.forNumber(42L).value shouldBe a[java.lang.Long]
      }

      "should handle -0" in {
        RemoteObject.forNumber(-0d) should be (RemoteObject("number", null, null, "-0", null, "-0", null))
      }
    }

    "trueValue should be a boolean" in {
      RemoteObject.trueValue should be (RemoteObject("boolean", null, null, null, true, null, null))
    }

    "falseValue should be a boolean" in {
      RemoteObject.falseValue should be (RemoteObject("boolean", null, null, null, false, null, null))
    }

    "nullValue should be an object" in {
      RemoteObject.nullValue should be (RemoteObject("object", "null", null, null, null, null, null))
    }

    "undefinedValue should have type 'undefined'" in {
      RemoteObject.undefinedValue should be (RemoteObject("undefined", null, null, null, null, null, null))
    }

    "forString" - {
      "should handle a string" in {
        RemoteObject.forString("test") should be (RemoteObject("string", null, null, null, "test", null, null))
      }

      "should handle null" in {
        RemoteObject.forString(null) should be (RemoteObject.nullValue)
      }
    }

    "forArray" - {
      "should handle an empty array" in {
        RemoteObject.forArray(0, "arr-id") should be (RemoteObject("object", "array", "Array", "Array[0]", null, null, "arr-id"))
      }

      "should handle an array with a size" in {
        RemoteObject.forArray(3, "arr-id") should be (RemoteObject("object", "array", "Array", "Array[3]", null, null, "arr-id"))
      }

      "should reject a negative size" in {
        assertThrows[IllegalArgumentException](RemoteObject.forArray(-1, "arr-id"))
      }

      "should reject null object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forArray(1, null))
      }

      "should reject empty object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forArray(1, ""))
      }

      "should accept an Array" in {
        val data = Seq("foo")
        RemoteObject.forArray(data) should be (RemoteObject("object", "array", "Array", "Array[1]", data, null, null))
      }
    }

    "forObject" - {
      "should handle an object" in {
        RemoteObject.forObject("an-id") should be (RemoteObject("object", null, "Object", "Object", null, null, "an-id"))
      }

      "should reject null object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forObject(null.asInstanceOf[String]))
      }

      "should reject empty object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forObject(""))
      }

      "should accept an object Map" in {
        val data = Map("foo" -> "bar")
        RemoteObject.forObject(data) should be (RemoteObject("object", null, "Object", "Object", data, null, null))
      }
    }

    "forFunction" - {
      "should handle name and source (and assume source includes the entire function definition)" in {
        RemoteObject.forFunction("fun", "function fun() { return 42; }", "an-id") should be (RemoteObject("function", null, "Function", "function fun() { return 42; }", null, null, "an-id"))
      }

      "should handle unknown/null source" in {
        RemoteObject.forFunction("fun", null, "an-id") should be (RemoteObject("function", null, "Function", "function fun() { [unknown] }", null, null, "an-id"))
      }

      "should reject null object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forFunction("fun", null, null))
      }

      "should reject empty object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forFunction("fun", null, ""))
      }
    }

    "forError" - {
      "should create an object with type 'object'" in {
        val ro = RemoteObject.forError("Error", "oops", None, "an-id")
        ro.`type` should be ("object")
      }

      "should create an object with subtype 'error'" in {
        val ro = RemoteObject.forError("Error", "oops", None, "an-id")
        ro.subtype should be ("error")
      }

      "should create an object with class name from the name" in {
        val ro = RemoteObject.forError("SomeError", "oops", None, "an-id")
        ro.className should be ("SomeError")
      }

      "should create an object with a description based on name and message if there is no stack" in {
        val ro = RemoteObject.forError("SomeError", "oops", None, "an-id")
        ro.description should be ("SomeError: oops")
      }

      "should create an object with a description based on the stack if there is one" in {
        val ro = RemoteObject.forError("SomeError", "oops", Some("stack"), "an-id")
        ro.description should be ("stack")
      }
    }

    "forDate" - {
      "should create an object based on a string representation" in {
        val stringRep = "Thu Dec 29 2016 23:29:30 GMT+0100 (CET)"
        val ro = RemoteObject.forDate(stringRep, "an-id")
        ro should be (RemoteObject("object", "date", "Date", stringRep, null, null, "an-id"))
      }
    }
  }
}
