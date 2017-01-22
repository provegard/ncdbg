package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, Undefined}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.Inside

class RemoteObjectConverterTest extends UnitTest with Inside {

  def converter = new RemoteObjectConverter()

  def lazyNode(v: ValueNode) = new LazyNode {
    override def resolve(): ValueNode = v
  }

  "RemoteObjectConverter should" - {
    "convert EmptyNode to RemoteObject.null" in {
      converter.toRemoteObject(EmptyNode, byValue = false) should be (RemoteObject.nullValue)
    }

    "convert SimpleValue with a string to a string-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue("test"), byValue = false) should be (RemoteObject.forString("test"))
    }

    "convert SimpleValue with 'true' to a boolean-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(true), byValue = false) should be (RemoteObject.trueValue)
    }

    "convert SimpleValue with 'false' to a boolean-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(false), byValue = false) should be (RemoteObject.falseValue)
    }

    "convert SimpleValue with a double-number to a number-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(42.0d), byValue = false) should be (RemoteObject.forNumber(42.0d))
    }

    "convert SimpleValue with an integer-number to an integer-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(42), byValue = false).value shouldBe a[java.lang.Integer]
    }

    "convert SimpleValue with a long-number to a long-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(42L), byValue = false).value shouldBe a[java.lang.Long]
    }

    "convert SimpleValue with Undefined to RemoteObject.undefined" in {
      converter.toRemoteObject(SimpleValue(Undefined), byValue = false) should be (RemoteObject.undefinedValue)
    }

    "convert ArrayNode to an array-based RemoteObject with a JSON object Id" in {
      val lzy = lazyNode(SimpleValue("s"))
      val arr = ArrayNode(Seq(lzy), ObjectId("obj-1"))

      converter.toRemoteObject(arr, byValue = false) should be (RemoteObject.forArray(1, """{"id":"obj-1"}"""))
    }

    "convert ObjectNode to an object-based RemoteObject with a JSON object Id" in {
      val obj = ObjectNode(Map.empty, ObjectId("obj-1"))

      converter.toRemoteObject(obj, byValue = false) should be (RemoteObject.forObject("""{"id":"obj-1"}"""))
    }

    "convert FunctionNode to an function-based RemoteObject with a JSON object Id" in {
      val fun = FunctionNode("fun1", "return 'test';", Map.empty, ObjectId("obj-1"))

      converter.toRemoteObject(fun, byValue = false) should be (RemoteObject.forFunction("fun1", "return 'test';", """{"id":"obj-1"}"""))
    }

    "convert ErrorValue to a RemoteObject with error details" in {
      val data = ExceptionData("SomeError", "oops", 10, 0, "file:/tmp/data.js", Some("SomeError: oops"), None)
      val ev = ErrorValue(data, isBasedOnThrowable = false, ObjectId("obj-1"))
      converter.toRemoteObject(ev, byValue = false) should be (RemoteObject.forError("SomeError", "oops", Some("SomeError: oops"), """{"id":"obj-1"}"""))
    }

    "convert DateNode to a RemoteObject with a string representation of the date" in {
      val stringRep = "Thu Dec 29 2016 23:29:30 GMT+0100 (CET)"
      val date = DateNode(stringRep, ObjectId("obj-1"))
      converter.toRemoteObject(date, byValue = false).description should be (stringRep)
    }

    "convert ObjectNode by-value to an object value without object ID" in {
      val data = Map("foo" -> LazyNode.eager(SimpleValue("bar")))
      val obj = ObjectNode(data, ObjectId("obj-1"))

      converter.toRemoteObject(obj, byValue = true) should be (RemoteObject.forObject(Map("foo" -> "bar")))
    }

    "convert ArrayNode by-value to an array value without object ID" in {
      val data = Seq(LazyNode.eager(SimpleValue("bar")))
      val arr = ArrayNode(data, ObjectId("obj-1"))

      converter.toRemoteObject(arr, byValue = true) should be (RemoteObject.forArray(Array("bar")))
    }
  }
}
