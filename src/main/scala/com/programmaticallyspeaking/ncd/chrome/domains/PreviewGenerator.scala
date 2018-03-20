package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.PreviewGenerator.{Options, PropertyFetcher}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{EntryPreview, ObjectPreview, PropertyPreview, RemoteObject}
import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.host._

object PreviewGenerator {
  type PropertyFetcher = (ObjectId) => Seq[(String, ObjectPropertyDescriptor)]

  case class Options(maxStringLength: Int, maxProperties: Int, maxIndices: Int, maxEntries: Int)

  val DefaultOptions = Options(100, 5, 100, 5)

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

  val scopeDesc = "^([^ ]*)( \\((.*)\\))?$".r
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
  import com.programmaticallyspeaking.ncd.infra.StringUtils._

  private val converter = RemoteObjectConverter.byReference

  def withPreviewForObject(remoteObject: RemoteObject): RemoteObject = remoteObject.objectId match {
    case Some(_) if RemoteObject.isScope(remoteObject) => generateScopePreview(remoteObject)
    case Some(_) if remoteObject.`type` == "object" => generatePreview(remoteObject)
    case _ => remoteObject
  }

  private def generatePreview(obj: RemoteObject): RemoteObject = {
    val preview = obj.emptyPreview

    obj.objectId match {
      case Some(id) =>
        val objectId = ObjectId.fromString(id)
        val props = propertyFetcher(objectId)
        if (props == null) throw new IllegalStateException(s"No properties returned for object $objectId")
        obj.copy(preview = Some(appendPropertyDescriptors(obj, preview, props)))
      case None =>
        //throw new IllegalArgumentException("Missing object ID for " + obj)
        obj.copy(preview = Some(preview))
    }
  }

  private def generateScopePreview(remoteObject: RemoteObject): RemoteObject = {
    remoteObject.description match {
      case Some(scopeDesc(typ, _, nameNull)) =>
        val name = Option(nameNull).getOrElse("")

        val props = Seq(
          PropertyPreview("name", "string", name, None),
          PropertyPreview("type", "string", typ.toLowerCase, None),
          PropertyPreview("object", "object", "Object", None) // in Chrome, value can be 'Window'
        )

        val preview = remoteObject.emptyPreview.copy(properties = props)
        remoteObject.copy(preview = Some(preview))

      case other =>
        throw new IllegalArgumentException("Unexpected scope description: " + other)
    }
  }

  private def appendPropertyDescriptors(obj: RemoteObject, preview: ObjectPreview, props: Seq[(String, ObjectPropertyDescriptor)]): ObjectPreview = {
    props.foldLeft(preview)((old, nameAndDescriptor) => appendPropertyDescriptor(obj, old, nameAndDescriptor._1, nameAndDescriptor._2))
  }

  private def reachedMax(preview: ObjectPreview, propertyName: String) = {
    val max = if (isUnsignedInt(propertyName)) options.maxIndices else options.maxProperties
    preview.properties.size == max
  }

  private def appendEntries(remoteObject: RemoteObject, preview: ObjectPreview, descriptor: ObjectPropertyDescriptor): ObjectPreview = {
    descriptor.value match {
      case Some(an: ArrayNode) =>
        val entryNodes = propertyFetcher(an.objectId).map(_._2.value).collect { case Some(e: MapSetEntryNode) => e }
        // Preserve existing overflow flag. Pure speculation - I don't know if it matters.
        val isOverflow = preview.overflow || entryNodes.size > options.maxEntries

        val entryPreview = entryNodes.take(options.maxEntries).map { e =>
          val keyPreview = e.key.map(converter.toRemoteObject).map(generatePreview).flatMap(_.preview)
          val valuePreview = generatePreview(converter.toRemoteObject(e.value)).preview.get
          EntryPreview(keyPreview, valuePreview)
        }

        preview.copy(entries = entryPreview, overflow = isOverflow)

      case other => throw new IllegalStateException("Unexpected entries value: " + other)
    }
  }

  private def appendPropertyDescriptor(obj: RemoteObject, preview: ObjectPreview, name: String, descriptor: ObjectPropertyDescriptor): ObjectPreview = {
    if (preview.overflow || !shouldUse(obj, name, descriptor)) return preview

    if (name == "[[Entries]]") {
      return appendEntries(obj, preview, descriptor)
    }

    // If adding this property would return in overflow, mark the preview and return it otherwise unchanged.
    if (reachedMax(preview, name)) {
      return preview.copy(overflow = true)
    }

    // TODO: descriptor.wasThrown
    val propertyPreview = descriptor.value match {
      case Some(EmptyNode) =>
        // { name: name, type: "object", subtype: "null", value: "null", __proto__: null }
        PropertyPreview(name, "object", "null", Some("null"))
      case Some(SimpleValue(s: String)) if s.length > options.maxStringLength =>
        // Abbreviate long strings
        PropertyPreview(name, "string", abbreviateString(s, options.maxStringLength, middle = false), None)
      case Some(value) =>
        val valueAsRemote = converter.toRemoteObject(value)

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

  private def isArrayLike(obj: RemoteObject) =
    obj.subtype.contains("array") || obj.subtype.contains("typedarray")

  private def shouldUse(obj: RemoteObject, name: String, descriptor: ObjectPropertyDescriptor): Boolean = name match {
    case "__proto__" =>
      // Ignore __proto__ property.
      false
    case prop if isArrayLike(obj) && !isUnsignedInt(prop) =>
      // For arrays, only include indices
      false
//    case "size" if obj.subtype.contains("map") || obj.subtype.contains("set") =>
//      // Ignore size property of map, set.
//      false
    case _ if !descriptor.isOwn =>
      // Never preview prototype properties.
      false
    case _ =>
      descriptor.value match {
        case Some(_: FunctionNode) =>
          // Never render functions in object preview.
          // But, array of functions is ok!
          obj.subtype.contains("array") && isUnsignedInt(name)

        case Some(_) =>
          true

        case None =>
          // Ignore computed properties.
          false
      }
  }
}
