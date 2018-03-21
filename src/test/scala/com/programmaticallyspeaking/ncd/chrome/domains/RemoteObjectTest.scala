package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host.{MapSetEntryNode, SimpleValue}
import com.programmaticallyspeaking.ncd.testing.UnitTest

class RemoteObjectTest extends UnitTest {
  import Runtime._

  "RemoteObject" - {
    "forNumber" - {
      "should handle a normal number" in {
        RemoteObject.forNumber(42.0d) should be (RemoteObject("number", None, None, Some("42.0"), Some(42.0d), None, None))
      }

      "should handle NaN" in {
        RemoteObject.forNumber(Double.NaN) should be (RemoteObject("number", None, None, Some("NaN"), None, Some("NaN"), None))
      }

      "should handle positive Infinity" in {
        RemoteObject.forNumber(Double.PositiveInfinity) should be (RemoteObject("number", None, None, Some("Infinity"), None, Some("Infinity"), None))
      }

      "should handle negative Infinity" in {
        RemoteObject.forNumber(Double.NegativeInfinity) should be (RemoteObject("number", None, None, Some("-Infinity"), None, Some("-Infinity"), None))
      }

      "should handle 0 (integer)" in {
        RemoteObject.forNumber(0) should be (RemoteObject("number", None, None, Some("0"), Some(0), None, None))
      }

      "should handle a long number" in {
        RemoteObject.forNumber(42L).value.map(_.getClass.getName) shouldBe Some("java.lang.Long")
      }

      "should handle -0" in {
        RemoteObject.forNumber(-0d) should be (RemoteObject("number", None, None, Some("-0"), None, Some("-0"), None))
      }
    }

    "trueValue should be a boolean" in {
      RemoteObject.trueValue should be (RemoteObject("boolean", None, None, None, Some(true), None, None))
    }

    "falseValue should be a boolean" in {
      RemoteObject.falseValue should be (RemoteObject("boolean", None, None, None, Some(false), None, None))
    }

    "NoneValue should be an object" in {
      RemoteObject.nullValue should be (RemoteObject("object", Some("null"), None, None, Some(null), None, None))
    }

    "undefinedValue should have type 'undefined'" in {
      RemoteObject.undefinedValue should be (RemoteObject("undefined", None, None, None, None, None, None))
    }

    "forString" - {
      "should handle a string" in {
        RemoteObject.forString("test") should be (RemoteObject("string", None, None, None, Some("test"), None, None))
      }

      "should handle null" in {
        RemoteObject.forString(null) should be (RemoteObject.nullValue)
      }
    }

    "forSymbol" - {
      "should handle description and object ID" in {
        RemoteObject.forSymbol("Symbol(foo)", "sym-id") should be (RemoteObject("symbol", None, None, Some("Symbol(foo)"), None, None, Some("sym-id")))
      }
    }

    "forMap" - {
      "should handle a Map" in {
        RemoteObject.forMap(3, weak = false, "map-id") should be (RemoteObject("object", Some("map"), Some("Map"), Some("Map(3)"), None, None, Some("map-id")))
      }

      "should handle a WeakMap" in {
        RemoteObject.forMap(-1, weak = true, "map-id") should be (RemoteObject("object", Some("weakmap"), Some("WeakMap"), Some("WeakMap"), None, None, Some("map-id")))
      }
    }

    "forSet" - {
      "should handle a Set" in {
        RemoteObject.forSet(3, weak = false, "set-id") should be (RemoteObject("object", Some("set"), Some("Set"), Some("Set(3)"), None, None, Some("set-id")))
      }

      "should handle a WeakSet" in {
        RemoteObject.forSet(-1, weak = true, "set-id") should be (RemoteObject("object", Some("weakset"), Some("WeakSet"), Some("WeakSet"), None, None, Some("set-id")))
      }
    }

    "forScopes" - {
      "should handle a length" in {
        RemoteObject.forScopes(3, "dummy") should be (
          RemoteObject("object", Some("internal#scopeList"), Some("Array"), Some("Scopes[3]"), None, None, Some("dummy"))
        )
      }
    }

    "forScope" - {
      "should handle empty name" in {
        RemoteObject.forScope("closure", "", "dummy") should be (
          RemoteObject("object", Some("internal#scope"), Some("Object"), Some("Closure"), None, None, Some("dummy"))
        )
      }

      "should handle non-empty name" in {
        RemoteObject.forScope("closure", "Foo", "dummy") should be (
          RemoteObject("object", Some("internal#scope"), Some("Object"), Some("Closure (Foo)"), None, None, Some("dummy"))
        )
      }
    }

    "isScope" - {
      "should accept a RemoteObject that is created with forScope" in {
        val ro = RemoteObject.forScope("closure", "Foo", "dummy")
        RemoteObject.isScope(ro) should be (true)
      }

      "should reject a non-scope (string)" in {
        val ro = RemoteObject.forString("foo")
        RemoteObject.isScope(ro) should be (false)
      }

      "should reject a non-scope (typed array, i.e. with subtype)" in {
        val ro = RemoteObject.forArray(3, Some("Int8Array"), "dummy")
        RemoteObject.isScope(ro) should be (false)
      }
    }

    "forArray" - {
      "should handle an empty array" in {
        RemoteObject.forArray(0, None, "arr-id") should be (RemoteObject("object", Some("array"), Some("Array"), Some("Array[0]"), None, None, Some("arr-id")))
      }

      "should handle an array with a size" in {
        RemoteObject.forArray(3, None, "arr-id") should be (RemoteObject("object", Some("array"), Some("Array"), Some("Array[3]"), None, None, Some("arr-id")))
      }

      "should handle a typed array with a size" in {
        RemoteObject.forArray(3, Some("Int8Array"), "arr-id") should be (
          RemoteObject("object", Some("typedarray"), Some("Int8Array"), Some("Int8Array[3]"), None, None, Some("arr-id")))
      }

      "should reject a negative size" in {
        assertThrows[IllegalArgumentException](RemoteObject.forArray(-1, None, "arr-id"))
      }

      "should reject null object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forArray(1, None, null))
      }

      "should reject empty object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forArray(1, None, ""))
      }

      "should accept an Array" in {
        val data = Seq("foo")
        RemoteObject.forArray(data) should be (RemoteObject("object", Some("array"), Some("Array"), Some("Array[1]"), Some(data), None, None))
      }
    }

    "forObject" - {
      "should handle an object" in {
        RemoteObject.forObject("Object", "an-id") should be (RemoteObject("object", None, Some("Object"), Some("Object"), None, None, Some("an-id")))
      }

      "should use the class name" in {
        RemoteObject.forObject("ArrayBuffer", "an-id").className should be (Some("ArrayBuffer"))
      }

      "should use the class name as description also" in {
        RemoteObject.forObject("ArrayBuffer", "an-id").description should be (Some("ArrayBuffer"))
      }

      "should reject null object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forObject("Object", null.asInstanceOf[String]))
      }

      "should reject empty object ID" in {
        assertThrows[IllegalArgumentException](RemoteObject.forObject("Object", ""))
      }

      "should accept an object Map" in {
        val data = Map("foo" -> "bar")
        RemoteObject.forObject(data) should be (RemoteObject("object", None, Some("Object"), Some("Object"), Some(data), None, None))
      }
    }

    "forFunction" - {
      "should handle name and source (and assume source includes the entire function definition)" in {
        RemoteObject.forFunction("fun", "function fun() { return 42; }", "an-id") should be (RemoteObject("function", None,
          Some("Function"), Some("function fun() { return 42; }"), None, None, Some("an-id")))
      }

      "should handle unknown/null source" in {
        RemoteObject.forFunction("fun", null, "an-id") should be (RemoteObject("function", None, Some("Function"),
          Some("function fun() { [unknown] }"), None, None, Some("an-id")))
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
        ro.subtype should be (Some("error"))
      }

      "should create an object with class name from the name" in {
        val ro = RemoteObject.forError("SomeError", "oops", None, "an-id")
        ro.className should be (Some("SomeError"))
      }

      "should create an object with a description based on name and message if there is no stack" in {
        val ro = RemoteObject.forError("SomeError", "oops", None, "an-id")
        ro.description should be (Some("SomeError: oops"))
      }

      "should create an object with a description based on the stack if there is one" in {
        val ro = RemoteObject.forError("SomeError", "oops", Some("stack"), "an-id")
        ro.description should be (Some("stack"))
      }
    }

    "forDate" - {
      "should create an object based on a string representation" in {
        val stringRep = "Thu Dec 29 2016 23:29:30 GMT+0100 (CET)"
        val ro = RemoteObject.forDate(stringRep, "an-id")
        ro should be (RemoteObject("object", Some("date"), Some("Date"), Some(stringRep), None, None, Some("an-id")))
      }
    }

    "forRegExp" - {
      "should create an object based on a string representation" in {
        val stringRep = "/.*/"
        val ro = RemoteObject.forRegExp(stringRep, "an-id")
        ro should be (RemoteObject("object", Some("regexp"), Some("RegExp"), Some(stringRep), None, None, Some("an-id")))
      }
    }

    "forMapEntry" - {
      "creates an object when there is a key" in {
        val ro = RemoteObject.forMapEntry("a", "b", "an-id")
        ro should be (RemoteObject("object", Some("internal#entry"), Some("Object"), Some("a => b"), None, None, Some("an-id")))
      }
    }

    "forSetEntry" - {
      "creates an object when there is a key" in {
        val ro = RemoteObject.forSetEntry("a", "an-id")
        ro should be (RemoteObject("object", Some("internal#entry"), Some("Object"), Some("a"), None, None, Some("an-id")))
      }
    }
  }
}
