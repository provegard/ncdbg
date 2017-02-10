package com.programmaticallyspeaking.ncd.nashorn

import java.util.Collections

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.testing.UnitTest
import com.sun.jdi._
import com.sun.jdi.request.{ClassPrepareRequest, EventRequestManager}
import org.scalatest.mockito.MockitoSugar

import scala.collection.mutable

class MarshallerTest extends UnitTest with MockitoSugar {
  import org.mockito.Mockito._

  import scala.collection.JavaConverters._

  val registered = mutable.Map[ObjectId, (Value, ComplexNode)]()
  val mappingRegistry = new MappingRegistry {
    override def register(value: Value, valueNode: ComplexNode, extra: Map[String, ValueNode]): Unit = {
      registered += valueNode.objectId -> (value, valueNode)
    }
  }
  def fakeThread = {
    val erm = mock[EventRequestManager]
    val vm = mock[VirtualMachine]
    when(vm.eventRequestManager()).thenReturn(erm)
    when(erm.classPrepareRequests()).thenReturn(Collections.emptyList[ClassPrepareRequest]())
    val thread = mock[ThreadReference]
    when(thread.virtualMachine()).thenReturn(vm)
    thread
  }
  def newMarshaller = new Marshaller(fakeThread, mappingRegistry)
  
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

  "Marshaller.marshalledAs" - {
    "should extract a string" in {
      val value = stringRef("test")
      newMarshaller.marshalledAs[String](value) should be ("test")
    }

    "should extract a Scala Boolean" in {
      val value = booleanValue(true)
      newMarshaller.marshalledAs[Boolean](value) should be (true)
    }

    "should throw if trying to extract a value of the wrong type" in {
      val value = booleanValue(true)
      assertThrows[ClassCastException](newMarshaller.marshalledAs[String](value))
    }
  }

  "Marshaller.marshalledAsOptionally" - {
    "should extract a string" in {
      val value = stringRef("test")
      newMarshaller.marshalledAsOptionally[String](value) should be (Some("test"))
    }

    "should return None if trying to extract a value of the wrong type" in {
      val value = booleanValue(true)
      newMarshaller.marshalledAsOptionally[String](value) should be (None)
    }
  }

  private def arrayOfStrings(strings: Seq[String]): ArrayReference = {
    val ar = mock[ArrayReference]
    val list: java.util.List[Value] = strings.map(s => stringRef(s).asInstanceOf[Value]).asJava
    when(ar.getValues).thenReturn(list)
    ar
  }

  private def stringRef(s: String): StringReference = {
    val sr = mock[StringReference]
    when(sr.value()).thenReturn(s)
    sr
  }

  private def booleanValue(value: Boolean): PrimitiveValue = {
    val pv = mock[BooleanValue]
    when(pv.booleanValue()).thenReturn(value.booleanValue())
    pv
  }
}
