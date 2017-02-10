package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, ObjectPropertyDescriptor, Undefined}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.testing.{MapBasedObjectInteraction, UnitTest}
import org.scalatest.Inside

class RemoteObjectConverterTest extends UnitTest with Inside {

  def lazyNode(v: ValueNode) = new LazyNode {
    override def resolve(): ValueNode = v
  }

  "RemoteObjectConverter should" - {
    def converter = RemoteObjectConverter.byReference

    "convert EmptyNode to RemoteObject.null" in {
      converter.toRemoteObject(EmptyNode) should be (RemoteObject.nullValue)
    }

    "convert SimpleValue with a string to a string-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue("test")) should be (RemoteObject.forString("test"))
    }

    "convert SimpleValue with 'true' to a boolean-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(true)) should be (RemoteObject.trueValue)
    }

    "convert SimpleValue with 'false' to a boolean-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(false)) should be (RemoteObject.falseValue)
    }

    "convert SimpleValue with a double-number to a number-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(42.0d)) should be (RemoteObject.forNumber(42.0d))
    }

    "convert SimpleValue with an integer-number to an integer-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(42)).value.map(_.getClass.getName) shouldBe Some("java.lang.Integer")
    }

    "convert SimpleValue with a long-number to a long-based RemoteObject" in {
      converter.toRemoteObject(SimpleValue(42L)).value.map(_.getClass.getName) shouldBe Some("java.lang.Long")
    }

    "convert SimpleValue with Undefined to RemoteObject.undefined" in {
      converter.toRemoteObject(SimpleValue(Undefined)) should be (RemoteObject.undefinedValue)
    }

    "convert ArrayNode to an array-based RemoteObject with a JSON object Id" in {
      val lzy = lazyNode(SimpleValue("s"))
      val arr = ArrayNode(Seq(lzy), ObjectId("obj-1"))

      converter.toRemoteObject(arr) should be (RemoteObject.forArray(1, """{"id":"obj-1"}"""))
    }

    "convert ObjectNode to an object-based RemoteObject with a JSON object Id" in {
      val obj = ObjectNode(ObjectId("obj-1"))

      converter.toRemoteObject(obj) should be (RemoteObject.forObject("""{"id":"obj-1"}"""))
    }

    "convert FunctionNode to an function-based RemoteObject with a JSON object Id" in {
      val fun = FunctionNode("fun1", "return 'test';", ObjectId("obj-1"))

      converter.toRemoteObject(fun) should be (RemoteObject.forFunction("fun1", "return 'test';", """{"id":"obj-1"}"""))
    }

    "convert ErrorValue to a RemoteObject with error details" in {
      val data = ExceptionData("SomeError", "oops", 10, 0, "file:/tmp/data.js", Some("SomeError: oops"))
      val ev = ErrorValue(data, isBasedOnThrowable = false, ObjectId("obj-1"))
      converter.toRemoteObject(ev) should be (RemoteObject.forError("SomeError", "oops", Some("SomeError: oops"), """{"id":"obj-1"}"""))
    }

    "convert DateNode to a RemoteObject with a string representation of the date" in {
      val stringRep = "Thu Dec 29 2016 23:29:30 GMT+0100 (CET)"
      val date = DateNode(stringRep, ObjectId("obj-1"))
      converter.toRemoteObject(date).description should be (Some(stringRep))
    }

    "convert RegExpNode to a RemoteObject with a string representation of the regular expression" in {
      val stringRep = "/.*/"
      val regexp = RegExpNode(stringRep, ObjectId("obj-1"))
      converter.toRemoteObject(regexp).description should be (Some(stringRep))
    }
  }

  "Value-based RemoteObjectConverter should" - {
    def valueConverter[A <: ValueNode](objectId: ObjectId, data: Map[String, A]) = RemoteObjectConverter.byValue(new MapBasedObjectInteraction(Map(objectId -> data)))

    "convert ObjectNode by-value to an object value without object ID" in {
      val data = Map("foo" -> SimpleValue("bar"))
      val objectId = ObjectId("obj-1")
      val obj = ObjectNode(objectId)

      valueConverter(objectId, data).toRemoteObject(obj) should be (RemoteObject.forObject(Map("foo" -> "bar")))
    }

    "convert ArrayNode by-value to an array value without object ID" in {
      val data = Seq(LazyNode.eager(SimpleValue("bar")))
      val objectId = ObjectId("obj-1")
      val arr = ArrayNode(data, objectId)

      valueConverter(objectId, Map.empty).toRemoteObject(arr) should be (RemoteObject.forArray(Array("bar")))
    }
  }
}
