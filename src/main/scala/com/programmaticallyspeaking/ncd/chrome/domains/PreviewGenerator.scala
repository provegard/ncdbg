package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.PreviewGenerator.{Options, PropertyFetcher}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ObjectPreview, PropertyPreview, RemoteObject}
import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.host.{EmptyNode, FunctionNode, ObjectId, SimpleValue}

object PreviewGenerator {
  case class Threshold(indexes: Int, properties: Int) {
    def reached = indexes < 0 || properties < 0
  }

  type PropertyFetcher = (ObjectId) => Map[String, ObjectPropertyDescriptor]

  case class Options(maxStringLength: Int, maxProperties: Int, maxIndices: Int)

  val DefaultOptions = Options(100, 5, 100)
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
        PropertyPreview(name, "string", abbreviateString(s, middle = false), None)
      case Some(value) =>
        val valueAsRemote = converter.toRemoteObject(value, byValue = false)
        val tempPreview = valueAsRemote.emptyPreview
        PropertyPreview(name, tempPreview.`type`, tempPreview.description, tempPreview.subtype)
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
//    // Never preview prototype properties.
////    if (!descriptor.isOwn) return true
    case _ if !descriptor.isOwn =>
      // Never preview prototype properties.
      false
    case _ => true
//    case _ =>
//      descriptor.value match {
//        case Some(value: FunctionNode) =>
//          // Never render functions in object preview.
////          if (type === "function" && (this.subtype !== "array" || !isUInt32(name)))
////            continue;
//          // TODO: if name is an array index, apparently functions are ok!?
//          false
//
//        case Some(value) =>
//          true
//
//        case None =>
//          // Ignore computed properties.
//          false
//      }
  }

  private def abbreviateString(string: String, middle: Boolean): String = {
    if (string.length <= options.maxStringLength)
      return string
//    if (middle) {
//      var leftHalf = maxLength >> 1;
//      var rightHalf = maxLength - leftHalf - 1;
//      return string.substr(0, leftHalf) + "\u2026" + string.substr(string.length - rightHalf, rightHalf);
//    }
    // Append ellipsis (...)
    string.substring(0, options.maxStringLength) + "\u2026"
  }
}
