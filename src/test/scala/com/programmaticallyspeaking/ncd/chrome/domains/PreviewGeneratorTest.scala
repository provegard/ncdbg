package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ObjectPreview, PropertyPreview, RemoteObject}
import com.programmaticallyspeaking.ncd.host.{EmptyNode, ObjectId, SimpleValue, ValueNode}
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
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

  val propertyMaps: Map[String, Map[String, ObjectPropertyDescriptor]] = Map(
    objectIdString("null") -> Map("foo" -> valueDescriptor(null)),
    objectIdString("prim") -> Map("foo" -> valueDescriptor(42)),
    objectIdString("string") -> Map("foo" -> valueDescriptor("abcdefghij")),
    objectIdString("longstring") -> Map("foo" -> valueDescriptor("abcdefghijk")),
    objectIdString("toomanyprops") -> Map("foo" -> valueDescriptor(42), "bar" -> valueDescriptor(43), "baz" -> valueDescriptor(44)),
    objectIdString("withprotoprop") -> Map("foo" -> valueDescriptor(42), "bar" -> valueDescriptor(43, isOwn = false)),
    objectIdString("array2") -> Map("0" -> valueDescriptor(42), "1" -> valueDescriptor(43), "length" -> valueDescriptor(2)),
    objectIdString("withpropnamedunderscoreproto") -> Map("__proto__" -> valueDescriptor("dummy"))
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

    ("ignores property '__proto__'",
      RemoteObject.forObject(objectIdString("withpropnamedunderscoreproto")),
      Some(previewWithProperties())),

    ("ignores 'length' property of an array",
      RemoteObject.forArray(2, objectIdString("array2")),
      Some(arrayPreviewWithProperties(PropertyPreview("0", "number", "42", None), PropertyPreview("1", "number", "43", None))))
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
