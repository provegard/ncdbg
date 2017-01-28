package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.PreviewGenerator.{Options, PropertyFetcher}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ObjectPreview, PropertyPreview, RemoteObject}
import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.host.{EmptyNode, FunctionNode, ObjectId, SimpleValue}

import scala.util.{Failure, Success, Try}

object PreviewGenerator {
  case class Threshold(indexes: Int, properties: Int) {
    def reached = indexes < 0 || properties < 0
  }

  type PropertyFetcher = (ObjectId) => Map[String, ObjectPropertyDescriptor]

  case class Options(maxStringLength: Int, maxProperties: Int, maxIndices: Int)

  val DefaultOptions = Options(100, 5, 100)

  private[PreviewGenerator] def abbreviateString(string: String, maxLength: Int, middle: Boolean): String = {
    if (string.length <= maxLength)
      return string
    if (middle) {
      val leftHalf = maxLength / 2
      val rightHalf = string.length - leftHalf
      // Insert ellipsis (...) in the middle
      return string.substring(0, leftHalf) + "\u2026" + string.substring(rightHalf)
    }
    // Append ellipsis (...)
    string.substring(0, maxLength) + "\u2026"
  }

  private[PreviewGenerator] def isUnsignedInt(s: String) = Try(s.toInt) match {
    case Success(value) => value >= 0
    case Failure(_) => false
  }
}

/**
  * Generates a preview for a [[RemoteObject]] instance. The protocol documentation
  * (https://chromedevtools.github.io/debugger-protocol-viewer/1-2) has no details on this, so the code here is based
  * on:
  *
  * https://github.com/nodejs/node/blob/8f00455c5194154ce909ddac6488ae2e42976a4c/deps/v8_inspector/src/inspector/injected-script-source.js
  *
  * @param propertyFetcher source of properties for an object
  * @param options options for preview generation
  */
class PreviewGenerator(propertyFetcher: PropertyFetcher, options: Options) {
  import PreviewGenerator._

  val converter = new RemoteObjectConverter

  def withPreviewForObject(remoteObject: RemoteObject): RemoteObject = {
    if (remoteObject.`type` == "object") generatePreview(remoteObject)
    else remoteObject
  }

  private def generatePreview(obj: RemoteObject): RemoteObject = {
    val preview = obj.emptyPreview
    val threshold = Threshold(100, 5)

    val objectId = obj.objectId match {
      case Some(id) => ObjectId.fromString(id)
      case None => throw new IllegalArgumentException("Missing object ID for " + obj)
    }
    val props = propertyFetcher(objectId) //, onlyOwn = true, onlyAccessors = false)
    obj.copy(preview = Some(appendPropertyDescriptors(obj, preview, props, threshold)))
    // Internal and map/set/iterator entries not supported
  }

  private def appendPropertyDescriptors(obj: RemoteObject, preview: ObjectPreview, props: Map[String, ObjectPropertyDescriptor], threshold: Threshold): ObjectPreview = {
    props.foldLeft(preview)((old, nameAndDescriptor) => appendPropertyDescriptor(obj, old, nameAndDescriptor._1, nameAndDescriptor._2, threshold))
  }

  private def appendPropertyDescriptor(obj: RemoteObject, preview: ObjectPreview, name: String, descriptor: ObjectPropertyDescriptor, threshold: Threshold): ObjectPreview = {
    if (threshold.reached || !shouldUse(obj, name, descriptor)) return preview

    // TODO: descriptor.wasThrown
    val propertyPreview = descriptor.value match {
      case Some(EmptyNode) =>
        // { name: name, type: "object", subtype: "null", value: "null", __proto__: null }
        PropertyPreview(name, "object", "null", Some("null"))
      case Some(SimpleValue(s: String)) if s.length > options.maxStringLength =>
        // Abbreviate long strings
        PropertyPreview(name, "string", abbreviateString(s, options.maxStringLength, middle = false), None)
      case Some(value) =>
        val valueAsRemote = converter.toRemoteObject(value, byValue = false)
        val tempPreview = valueAsRemote.emptyPreview
        // For non-simple (non-primitive) values, the description should be empty. Dev Tools will use the type/subtype
        // to show something.
        val description = value match {
          case _: FunctionNode => ""
          case _ => abbreviateString(tempPreview.description, options.maxStringLength, middle = valueAsRemote.subtype.contains("regexp"))
        }
        PropertyPreview(name, tempPreview.`type`, description, tempPreview.subtype)
      case None => ??? //TODO
    }
    // TODO: THRESHOLD
    preview.copy(properties = preview.properties :+ propertyPreview)
  }

  private def shouldUse(obj: RemoteObject, name: String, descriptor: ObjectPropertyDescriptor): Boolean = name match {
    case "__proto__" =>
      // Ignore __proto__ property.
      false
    case "length" if obj.subtype.contains("array") => // || obj.subtype.contains("typedarray") =>
      // Ignore length property of array.
      false
//    case "size" if obj.subtype.contains("map") || obj.subtype.contains("set") =>
//      // Ignore size property of map, set.
//      false
    case _ if !descriptor.isOwn =>
      // Never preview prototype properties.
      false
    case _ =>
      descriptor.value match {
        case Some(value: FunctionNode) =>
          // Never render functions in object preview.
          // But, array of functions is ok!
          obj.subtype.contains("array") && isUnsignedInt(name)

        case Some(value) =>
          true

        case None =>
          // Ignore computed properties.
          false
      }
  }
}
