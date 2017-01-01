package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, Undefined}
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ValueNodeExtractorTest extends UnitTest {

  def newExtractor = new ValueNodeExtractor

  "ValueNodeExtractor" - {
    "should extract the value from a SimpleValue" in {
      val v = SimpleValue("test")
      newExtractor.extract(v) should be ("test")
    }

    "should convert Undefined in SimpleValue to null, since Undefined won't serialize well to JSON" in {
      val v = SimpleValue(Undefined)
      assert(newExtractor.extract(v) == null)
    }

    "should convert EmptyNode to null" in {
      assert(newExtractor.extract(EmptyNode) == null)
    }

    "should convert DateNode to its string representation" in {
      val v = DateNode("the date rep", ObjectId("x"))
      newExtractor.extract(v) should be ("the date rep")
    }

    "should extract a value wrapped in a LazyNode" in {
      val v = LazyNode.eager(SimpleValue("test"))
      newExtractor.extract(v) should be ("test")
    }

    "should convert ArrayNode to an array" in {
      val v1 = SimpleValue("test1")
      val v2 = SimpleValue("test2")
      val a = ArrayNode(Seq(v1, v2).map(LazyNode.eager), ObjectId("a"))
      newExtractor.extract(a) should be (Array("test1", "test2"))
    }

    "should convert ObjectNode to a Map" in {
      val v1 = SimpleValue("test1")
      val o = ObjectNode(Map("foo" -> LazyNode.eager(v1)), ObjectId("x"))
      newExtractor.extract(o) should be (Map("foo" -> "test1"))
    }

    "should convert FunctionNode to a string rep" in {
      val f = FunctionNode("foo", "", Map.empty, ObjectId("f"))
      newExtractor.extract(f) should be ("<function foo() {}>")
    }

    "should convert ErrorNode to a string rep" in {
      val e = ErrorValue(ExceptionData("TypeError", "oops", 0, 0, "", None), ObjectId("e"))
      newExtractor.extract(e) should be ("<TypeError: oops>")
    }

    "should detect a cycle via an object but emit an error string instead of throwing" in {
      var o: ObjectNode = null
      o = ObjectNode(Map("foo" -> new LazyNode {
        override def resolve(): ValueNode = o
      }), ObjectId("x"))
      newExtractor.extract(o) should be (Map("foo" -> "<Error: cycle detected for object 'x'>"))
    }

    "should detect a cycle via an array but emit an error string instead of throwing" in {
      var a: ArrayNode = null
      a = ArrayNode(Seq(new LazyNode {
        override def resolve(): ValueNode = a
      }), ObjectId("x"))
      newExtractor.extract(a) should be (Array("<Error: cycle detected for array 'x'>"))
    }
  }
}
