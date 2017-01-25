package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{PropertyDescriptor, RemoteObject}
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.host.{FunctionNode, ObjectId, ObjectNode}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.PropertyChecks

class PropertyDescriptorTest extends UnitTest with PropertyChecks {

  private val anObject = ObjectNode(Map.empty, ObjectId("x"))
  private val aFunction = FunctionNode("fun", "function(){}", Map.empty, ObjectId("x"))
  private val anotherFunction = FunctionNode("fun2", "function(){}", Map.empty, ObjectId("y"))

  "PropertyDescriptor from ObjectPropertyDescriptor" - {
    "should handle a generic descriptor" in {
      forAll { (configurable: Boolean, writable: Boolean, enumerable: Boolean, own: Boolean) =>
        val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Generic, configurable, enumerable, writable,
          own, None, None, None)
        val expected = PropertyDescriptor("test", writable, configurable, enumerable, own, None, None, None)
        PropertyDescriptor.from("test", desc) should be(expected)
      }
    }

    "should handle a data descriptor" in {
      forAll { (configurable: Boolean, writable: Boolean, enumerable: Boolean, own: Boolean) =>
        val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Data, configurable, enumerable, writable,
          own, Some(anObject), None, None)

        val remoteObj = RemoteObject.forObject(anObject.objectId.toString)

        val expected = PropertyDescriptor("data", writable, configurable, enumerable, own, Some(remoteObj), None, None)
        PropertyDescriptor.from("data", desc) should be(expected)
      }
    }

    "should handle an accessor descriptor" in {
      forAll { (configurable: Boolean, writable: Boolean, enumerable: Boolean, own: Boolean) =>
        val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, configurable, enumerable, writable,
          own, None, Some(aFunction), Some(anotherFunction))

        val remoteFun = RemoteObject.forFunction(aFunction.name, aFunction.source, aFunction.objectId.toString)
        val remoteFun2 = RemoteObject.forFunction(anotherFunction.name, anotherFunction.source, anotherFunction.objectId.toString)

        val expected = PropertyDescriptor("acc", writable, configurable, enumerable, own, None, Some(remoteFun), Some(remoteFun2))
        PropertyDescriptor.from("acc", desc) should be(expected)
      }
    }

    "should set the name properly for a generic descriptor" in {
      val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Generic, false, false, false, false, None, None, None)
      val expected = PropertyDescriptor("name", false, false, false, false, None, None, None)
      PropertyDescriptor.from("name", desc).name should be ("name")
    }

    "should set the name properly for a data descriptor" in {
      val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, false, false, false, Some(anObject), None, None)
      PropertyDescriptor.from("name", desc).name should be ("name")
    }

    "should set the name properly for an accessor descriptor" in {
      val desc = ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, false, false, false, false, None, Some(aFunction), None)
      PropertyDescriptor.from("name", desc).name should be ("name")
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
