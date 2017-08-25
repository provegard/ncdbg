package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{InternalPropertyDescriptor, PropertyDescriptor, RemoteObject}
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.host.{FunctionNode, ObjectId, ObjectNode}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.PropertyChecks

class InternalPropertyDescriptorTest extends UnitTest with PropertyChecks {

  private val anObject = ObjectNode("Object", ObjectId("x"))
  private val aFunction = FunctionNode("fun", "function(){}", ObjectId("x"))
  private val anotherFunction = FunctionNode("fun2", "function(){}", ObjectId("y"))

  implicit val remoteObjectConverter = RemoteObjectConverter.byReference

  "InternalPropertyDescriptor from ObjectPropertyDescriptor" - {
    "should handle a data descriptor" in {
      val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, false, false, false, Some(anObject), None, None)

      val remoteObj = RemoteObject.forObject("Object", anObject.objectId.toString)

      val expected = InternalPropertyDescriptor("data", Some(remoteObj))
      InternalPropertyDescriptor.from("data", desc) should be(Some(expected))
    }

    "should ignore an accessor descriptor" in {
      val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, false, false, false, false, None, Some(aFunction), None)

      InternalPropertyDescriptor.from("data", desc) should be('empty)
    }

    "should ignore a generic descriptor" in {
      val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Generic, false, false, false, false, None, None, None)

      InternalPropertyDescriptor.from("data", desc) should be('empty)
    }

    "should reject null name" in {
      val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Generic, false, false, false, false, None, None, None)
      assertThrows[IllegalArgumentException](PropertyDescriptor.from(null, desc))
    }

    "should reject empty name" in {
      val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Generic, false, false, false, false, None, None, None)
      assertThrows[IllegalArgumentException](PropertyDescriptor.from("", desc))
    }
  }
}
