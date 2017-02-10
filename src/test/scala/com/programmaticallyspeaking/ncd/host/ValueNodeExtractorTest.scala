package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, Undefined}
import com.programmaticallyspeaking.ncd.testing.{MapBasedObjectInteraction, UnitTest}

class ValueNodeExtractorTest extends UnitTest {

  def newExtractor(data: Map[ObjectId, Map[String, ValueNode]] = Map.empty) = new ValueNodeExtractor(new MapBasedObjectInteraction(data))

  "ValueNodeExtractor" - {
    "should extract the value from a SimpleValue" in {
      val v = SimpleValue("test")
      newExtractor().extract(v) should be ("test")
    }

    "should convert Undefined in SimpleValue to null, since Undefined won't serialize well to JSON" in {
      val v = SimpleValue(Undefined)
      assert(newExtractor().extract(v) == null)
    }

    "should convert EmptyNode to null" in {
      assert(newExtractor().extract(EmptyNode) == null)
    }

    "should convert DateNode to its string representation" in {
      val v = DateNode("the date rep", ObjectId("x"))
      newExtractor().extract(v) should be ("the date rep")
    }

    "should extract a value wrapped in a LazyNode" in {
      val v = LazyNode.eager(SimpleValue("test"))
      newExtractor().extract(v) should be ("test")
    }

    "should convert ArrayNode to an array" in {
      val v1 = SimpleValue("test1")
      val v2 = SimpleValue("test2")
      val a = ArrayNode(Seq(v1, v2).map(LazyNode.eager), ObjectId("a"))
      newExtractor().extract(a) should be (Array("test1", "test2"))
    }

    "should convert ObjectNode to a Map" in {
      val oid = ObjectId("x")
      val o = ObjectNode(oid)
      newExtractor(Map(oid -> Map("foo" -> SimpleValue("test1")))).extract(o) should be (Map("foo" -> "test1"))
    }

    "should convert FunctionNode to a string rep" in {
      val f = FunctionNode("foo", "", ObjectId("f"))
      newExtractor().extract(f) should be ("<function foo() {}>")
    }

    "should convert ErrorNode to a string rep" in {
      val e = ErrorValue(ExceptionData("TypeError", "oops", 0, 0, "", None), isBasedOnThrowable = false, ObjectId("e"))
      newExtractor().extract(e) should be ("<TypeError: oops>")
    }

    "should detect a cycle via an object but emit an error string instead of throwing" in {
      var o: ObjectNode = null
      val data = Map("foo" -> new LazyNode {
        override def resolve(): ValueNode = o
      })
      val objectId = ObjectId("x")
      o = ObjectNode(objectId)
      newExtractor(Map(objectId -> data)).extract(o) should be (Map("foo" -> "<Error: cycle detected for object 'x'>"))
    }

    "should detect a cycle via an array but emit an error string instead of throwing" in {
      var a: ArrayNode = null
      a = ArrayNode(Seq(new LazyNode {
        override def resolve(): ValueNode = a
      }), ObjectId("x"))
      newExtractor().extract(a) should be (Array("<Error: cycle detected for array 'x'>"))
    }
  }
}
