package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.testing.UnitTest
import com.sun.jdi._
import org.scalatest.mockito.MockitoSugar

import scala.collection.mutable

class MarshallerTest extends UnitTest with MockitoSugar {
  import org.mockito.Mockito._

  import scala.collection.JavaConversions._

  val registered = mutable.Map[ObjectId, (Value, ComplexNode)]()
  val mappingRegistry = new MappingRegistry {
    override def register(value: Value, valueNode: ValueNode): Unit = valueNode match {
      case c: ComplexNode =>
        registered += c.objectId -> (value, c)
      case _ =>
    }
  }
  def newMarshaller = new Marshaller(null, mappingRegistry)
  
  "Marshaller" - {
    "should marshal null" in {
      newMarshaller.marshal(null) should be (EmptyNode)
    }

    "should marshal a primitive value" - {
      "which is a Byte" in {
        val bv = mock[ByteValue]
        when(bv.byteValue()).thenReturn(42.asInstanceOf[Byte])
        newMarshaller.marshal(bv) should be (SimpleValue(42.asInstanceOf[Byte]))
      }
    }

    "should marshal a string" in {
      val sr = stringRef("test")
      newMarshaller.marshal(sr) should be (SimpleValue("test"))
    }

    "should marshal an array" - {
      "by returning the correct type" in {
        val ar = arrayOfStrings(Seq("a"))
        newMarshaller.marshal(ar) shouldBe an[ArrayNode]
      }

      "by mapping the values to lazy nodes" in {
        val ar = arrayOfStrings(Seq("a"))

        val result = newMarshaller.marshal(ar).asInstanceOf[ArrayNode]
        result.items.map(_.isInstanceOf[LazyNode]).headOption should be (Some(true))
      }

      "by correctly marshalling a lazy array item" in {
        val ar = arrayOfStrings(Seq("a"))

        val result = newMarshaller.marshal(ar).asInstanceOf[ArrayNode]
        result.items.headOption match {
          case Some(lv: LazyNode) =>
            lv.resolve() should be (SimpleValue("a"))
          case x => fail("unexpected: " + x)
        }
      }

      "and register the mapping in the registry" in {
        val ar = arrayOfStrings(Seq("a"))

        val result = newMarshaller.marshal(ar).asInstanceOf[ArrayNode]
        registered.get(result.objectId) should be (Some(ar, result))
      }
    }
  }

  private def arrayOfStrings(strings: Seq[String]): ArrayReference = {
    val ar = mock[ArrayReference]
    val list: java.util.List[Value] = strings.map(stringRef)
    when(ar.getValues).thenReturn(list)
    ar
  }

  private def stringRef(s: String): StringReference = {
    val sr = mock[StringReference]
    when(sr.value()).thenReturn(s)
    sr
  }
}
