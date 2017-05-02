package com.programmaticallyspeaking.ncd.nashorn

import java.util.Collections

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.testing.UnitTest
import com.sun.jdi._
import com.sun.jdi.request.{ClassPrepareRequest, EventRequestManager}
import org.scalatest.mockito.MockitoSugar

import scala.collection.mutable
import scala.reflect.ClassTag

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

  def newMarshaller = new Marshaller(mappingRegistry)(fakeThread)

  def mockValue[A <: Value : ClassTag](typeName: String = "DoesntMatter"): A = {
    val v = mock[A]
    val tp = new Type {
      override def signature(): String = ""
      override def name(): String = typeName
      override def virtualMachine(): VirtualMachine = ???
    }
    when(v.`type`()).thenReturn(tp)
    v
  }

  "Marshaller" - {
    "should marshal null" in {
      newMarshaller.marshal(null) should be (EmptyNode)
    }

    "should marshal undefined" in {
      val uv = mockValue[ObjectReference]("jdk.nashorn.internal.runtime.Undefined")
      newMarshaller.marshal(uv) should be (SimpleValue(Undefined))
    }

    "should marshal a primitive value" - {
      "which is a Byte" in {
        val bv = byteValue(42.asInstanceOf[Byte])
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

      "by just extracting the size (maybe this kind of marshalling should go away?)" in {
        val ar = arrayOfStrings(Seq("a", "b"))

        val result = newMarshaller.marshal(ar).asInstanceOf[ArrayNode]
        result.size should be (2)
      }

      "and register the mapping in the registry" in {
        val ar = arrayOfStrings(Seq("a"))

        val result = newMarshaller.marshal(ar).asInstanceOf[ArrayNode]
        registered.get(result.objectId) should be (Some(ar, result))
      }
    }
  }

  private def arrayOfStrings(strings: Seq[String]): ArrayReference = {
    val ar = mockValue[ArrayReference]()
    val list: java.util.List[Value] = strings.map(s => stringRef(s).asInstanceOf[Value]).asJava
    when(ar.getValues).thenReturn(list)
    when(ar.length()).thenReturn(strings.size)
    ar
  }

  private def stringRef(s: String): StringReference = {
    val sr = mockValue[StringReference]()
    when(sr.value()).thenReturn(s)
    sr
  }

  private def byteValue(value: Byte): PrimitiveValue = {
    val pv = mockValue[ByteValue]()
    when(pv.byteValue()).thenReturn(value.toByte)
    pv
  }
}
