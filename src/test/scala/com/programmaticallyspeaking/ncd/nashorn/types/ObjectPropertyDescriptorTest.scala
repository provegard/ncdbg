package com.programmaticallyspeaking.ncd.nashorn.types

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.PropertyDescriptor
import com.programmaticallyspeaking.ncd.host.{FunctionNode, ObjectId, ObjectNode}
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class ObjectPropertyDescriptorTest extends UnitTest with TableDrivenPropertyChecks {
  private val anObject = ObjectNode("Object", ObjectId("x"))
  private val aFunction = FunctionNode("fun", "function(){}", ObjectId("x"))

  val errorDescriptors =
    Table(
      ("description", "name", "input"),
      ("generic with value", "invalid",
        () => ObjectPropertyDescriptor(PropertyDescriptorType.Generic, false, false, false, false, Some(anObject), None, None)),
      ("generic with getter", "invalid",
        () => ObjectPropertyDescriptor(PropertyDescriptorType.Generic, false, false, false, false, None, Some(aFunction), None)),
      ("generic with setter", "invalid",
        () => ObjectPropertyDescriptor(PropertyDescriptorType.Generic, false, false, false, false, None, None, Some(aFunction))),
      ("data without value", "invalid",
        () => ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, false, false, false, None, None, None)),
      ("accessor without getter or setter", "invalid",
        () => ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, false, false, false, false, None, None, None)),
      ("accessor with value", "invalid",
        () => ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, false, false, false, false, Some(anObject), Some(aFunction), None)),
      ("data with getter", "invalid",
          () => ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, false, false, false, Some(anObject), Some(aFunction), None)),
      ("data with setter", "invalid",
        () => ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, false, false, false, Some(anObject), None, Some(aFunction)))
    )


  "ObjectPropertyDescriptor" - {
    forAll(errorDescriptors) { (desc, name, factory) =>
      s"rejects $desc" in {
        assertThrows[IllegalArgumentException](factory())
      }
    }
  }
}
