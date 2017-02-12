package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, Undefined}
import com.programmaticallyspeaking.ncd.testing.{MapBasedObjectInteraction, UnitTest}

import scala.collection.mutable

class ValueNodeExtractorTest extends UnitTest {

  def newExtractor(data: Map[ObjectId, Map[String, ValueNode]] = Map.empty) = new ValueNodeExtractor(new MapBasedObjectInteraction(data))
  // TODO: Duplicated from RemoteObjectConverterTest
  def arrayToMap[A <: ValueNode](data: Seq[A]): Map[String, ValueNode] = {
    val map = mutable.Map[String, ValueNode]()
    data.zipWithIndex.foreach(e => map += (e._2.toString -> e._1))
    map += ("length" -> SimpleValue(data.size))
    map.toMap
  }

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
      val a = ArrayNode(2, ObjectId("a"))
      newExtractor(Map(a.objectId -> arrayToMap(Seq(v1, v2)))).extract(a) should be (Array("test1", "test2"))
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
      val item = new LazyNode {
        override def resolve(): ValueNode = a
      }
      a = ArrayNode(1, ObjectId("x"))
      newExtractor(Map(a.objectId -> arrayToMap(Seq(item)))).extract(a) should be (Array("<Error: cycle detected for array 'x'>"))
    }
  }
}
