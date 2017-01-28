package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ObjectPreview, PropertyPreview, RemoteObject}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType, Undefined}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class PreviewGeneratorTest extends UnitTest with TableDrivenPropertyChecks {
  // These are set low for testing
  val maxStringLength = 10
  val maxProperties = 2
  val maxIndices = 3

  def objectIdString(id: String) = ObjectId(id).toString

  def valueDescriptor(value: Any, isOwn: Boolean = true) =
    ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, true, true, isOwn = isOwn, Some(value match {
      case node: ValueNode => node
      case other if other == null => EmptyNode
      case other => SimpleValue(other)
    }), None, None)

  val aFunction = FunctionNode("fun", "function fun() {}", ObjectId("fun"))

  val accessorDescriptor =
    ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, false, true, true, true, None, Some(aFunction), None)

  val propertyMaps: Map[String, Map[String, ObjectPropertyDescriptor]] = Map(
    objectIdString("null") -> Map("foo" -> valueDescriptor(null)),
    objectIdString("prim") -> Map("foo" -> valueDescriptor(42)),
    objectIdString("string") -> Map("foo" -> valueDescriptor("abcdefghij")),
    objectIdString("longstring") -> Map("foo" -> valueDescriptor("abcdefghijk")),
    objectIdString("toomanyprops") -> Map("foo" -> valueDescriptor(42), "bar" -> valueDescriptor(43), "baz" -> valueDescriptor(44)),
    objectIdString("withprotoprop") -> Map("foo" -> valueDescriptor(42), "bar" -> valueDescriptor(43, isOwn = false)),
    objectIdString("array2") -> Map("0" -> valueDescriptor(42), "1" -> valueDescriptor(43), "length" -> valueDescriptor(2)),
    objectIdString("withpropnamedunderscoreproto") -> Map("__proto__" -> valueDescriptor("dummy")),
    objectIdString("arrayofobject") -> Map("0" -> valueDescriptor(ObjectNode(Map.empty, ObjectId("obj")))),
    objectIdString("arrayoffunction") -> Map("0" -> valueDescriptor(aFunction)),
    objectIdString("arrayofundefined") -> Map("0" -> valueDescriptor(SimpleValue(Undefined))),
    objectIdString("objwithfunctionvalue") -> Map("foo" -> valueDescriptor(aFunction)),
    objectIdString("withcomputedprop") -> Map("foo" -> accessorDescriptor),
    objectIdString("objwithdate") -> Map("foo" -> valueDescriptor(DateNode("Sat Jan 28 2017 13:25:02 GMT+0100 (W. Europe Standard Time)", ObjectId("date")))),
    objectIdString("objwithregexp") -> Map("foo" -> valueDescriptor(RegExpNode("/[a-z0-9A-Z_]{3,5}.*[a-z]$/", ObjectId("regexp"))))
  )

  def previewWithProperties(propertyPreview: PropertyPreview*) =
    ObjectPreview("object", "Object", false, None, Seq(propertyPreview: _*))

  def arrayPreviewWithProperties(propertyPreview: PropertyPreview*) = {
    val length = propertyPreview.size
    ObjectPreview("object", s"Array[$length]", false, Some("array"), Seq(propertyPreview: _*))
  }

  // TODO: What's the description for a typed array? Investigate!
//  def typedarrayPreviewWithProperties(propertyPreview: PropertyPreview*) = {
//    val length = propertyPreview.size
//    ObjectPreview("object", s"Array[$length]", false, Some("typedarray"), Seq(propertyPreview: _*))
//  }

  def getPreview(obj: RemoteObject, props: Map[String, ObjectPropertyDescriptor]): Option[ObjectPreview] = {
    val generator = new PreviewGenerator(_ => props, PreviewGenerator.Options(maxStringLength, maxProperties, maxIndices))
    generator.withPreviewForObject(obj).preview
  }

  val testCases = Table(
    ("description", "object", "expected"),

    ("ignores non-object", RemoteObject.forNumber(42), None),

    ("handles null property",
      RemoteObject.forObject(objectIdString("null")),
      Some(previewWithProperties(PropertyPreview("foo", "object", "null", Some("null"))))),

    ("handles primitive property",
      RemoteObject.forObject(objectIdString("prim")),
      Some(previewWithProperties(PropertyPreview("foo", "number", "42", None)))),

    ("handles a string property with a string not exceeding the max length",
      RemoteObject.forObject(objectIdString("string")),
      Some(previewWithProperties(PropertyPreview("foo", "string", "abcdefghij", None)))),

    ("abbreviates a long string",
      RemoteObject.forObject(objectIdString("longstring")),
      Some(previewWithProperties(PropertyPreview("foo", "string", "abcdefghij\u2026", None)))),

    ("ignores prototype properties",
      RemoteObject.forObject(objectIdString("withprotoprop")),
      Some(previewWithProperties(PropertyPreview("foo", "number", "42", None)))),

    ("ignores computed properties",
      RemoteObject.forObject(objectIdString("withcomputedprop")),
      Some(previewWithProperties())),

    ("ignores property '__proto__'",
      RemoteObject.forObject(objectIdString("withpropnamedunderscoreproto")),
      Some(previewWithProperties())),

    ("ignores 'length' property of an array",
      RemoteObject.forArray(2, objectIdString("array2")),
      Some(arrayPreviewWithProperties(PropertyPreview("0", "number", "42", None), PropertyPreview("1", "number", "43", None)))),

    ("handles array of objects with description",
      RemoteObject.forArray(1, objectIdString("arrayofobject")),
      Some(arrayPreviewWithProperties(PropertyPreview("0", "object", "Object", None)))),

    ("handles array of functions with empty value for a function",
      RemoteObject.forArray(1, objectIdString("arrayoffunction")),
      Some(arrayPreviewWithProperties(PropertyPreview("0", "function", "", None)))),

    ("handles array of undefined",
      RemoteObject.forArray(1, objectIdString("arrayofundefined")),
      Some(arrayPreviewWithProperties(PropertyPreview("0", "undefined", "undefined", None)))),

    ("ignores an object property with a function value",
      RemoteObject.forObject(objectIdString("objwithfunctionvalue")),
      Some(previewWithProperties())),

    ("abbreviates a Date string representation",
      RemoteObject.forObject(objectIdString("objwithdate")),
      Some(previewWithProperties(PropertyPreview("foo", "object", "Sat Jan 28\u2026", Some("date"))))),

    ("abbreviates a RegExp string representation _in the middle_",
      RemoteObject.forObject(objectIdString("objwithregexp")),
      Some(previewWithProperties(PropertyPreview("foo", "object", "/[a-z\u2026-z]$/", Some("regexp")))))
  )

  "Preview generation" - {
    forAll(testCases) { (desc, obj, expected) =>
      desc in {
        val props = obj.objectId.flatMap(propertyMaps.get).getOrElse(Map.empty)
        getPreview(obj, props) should be (expected)
      }
    }
  }
}
