package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.host.{ComplexNode, ObjectId, ObjectNode}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost._
import com.sun.jdi.{ArrayReference, ObjectReference, ThreadReference, Value}
import org.slf4s.Logging

import scala.util.{Failure, Success}

trait ObjectPropertiesSupport extends NashornScriptHost { self: NashornDebuggerHost with Logging =>
  import NashornDebuggerHost._

  private var maybeScriptBasedPropertyHolderFactory: Option[ScriptBasedPropertyHolderFactory] = None
  private var objectPropertiesCacheEnabled = true

  override def disableObjectPropertiesCache(): Unit = objectPropertiesCacheEnabled = false

  override def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)] = pausedData match {
    case Some(pd) =>
      implicit val marshaller = pd.marshaller
      implicit val thread = marshaller.thread

      val scopeObjectIds: Seq[ObjectId] = pd.stackFrames.flatMap(_.scopeChain).map(_.value).collect{case o: ObjectNode => o.objectId}
      val isScopeObject = scopeObjectIds.contains(objectId)

      // For scope objects, DevTools passes onlyOwn==false, but manual testing shows that Chrome itself only returns
      // the scope-own properties. Perhaps scopes aren't prototypically related in Chrome?
      val actualOnlyOwn = onlyOwn || isScopeObject

      // We're not interested in the __proto__ property for scope objects.
      val includeProto = !isScopeObject

      objectDescriptorById.get(objectId) match {
        case Some(desc: ObjectDescriptor) =>
          // Get object properties, via a cache.
          val cacheKey = ObjectPropertiesKey(objectId, actualOnlyOwn, onlyAccessors)
          def getProperties = createPropertyHolder(objectId, desc, includeProto).map(_.properties(actualOnlyOwn, onlyAccessors)).getOrElse(Seq.empty)
          val objectProperties = if (objectPropertiesCacheEnabled)
            pausedData.get.objectPropertiesCache.getOrElseUpdate(cacheKey, getProperties)
          else getProperties

          // In addition, the node may contain extra entries that may not come from Nashorn. One example is
          // the Java stack we add if we detect a Java exception. We also use this for internal properties!
          val extraProps = desc.extras.filterNot(_._1.startsWith(hiddenPrefix)).map(e => {
            ObjectPropertyDescriptor.toInternal(e._1) -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true, isWritable = false,
              isOwn = true, Some(e._2), None, None)
          })

          // Combine the two maps
          objectProperties ++ extraProps

        case None =>
          log.warn (s"Unknown object ($objectId), cannot get properties")
          Seq.empty
      }
    case None =>
      throw new IllegalStateException("Property extraction can only be done in a paused state.")
  }

  private def createPropertyHolder(objectId: ObjectId, objectDescriptor: ObjectDescriptor, includeProto: Boolean)(implicit marshaller: Marshaller): Option[PropertyHolder] = {
    val cache = pausedData.get.propertyHolderCache
    implicit val thread = marshaller.thread

    def scriptObjectHolder(ref: ObjectReference) = {
      var blacklistParts = Seq(hiddenPrefixEscapedForUseInJavaScriptRegExp + ".+")
      if (!includeProto) blacklistParts +:= "__proto__"
      val propBlacklistRegex = blacklistParts.map("(" + _ + ")").mkString("^", "|", "$")
      val factory = scriptBasedPropertyHolderFactory()
      val holder = factory.create(ref, propBlacklistRegex, isNative = true)
      new PropertyHolder {
        override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)] = {
          holder.properties(onlyOwn, onlyAccessors).map(accessorToDataForLocals(objectId))
        }
      }
    }

    //TODO: We can use scriptObjectHolder for ScriptObjectMirror also, but when do we one of those?
    cache.getOrElseUpdate(objectId, {
      objectDescriptor.native collect {
        case ref: ObjectReference if marshaller.isScriptObject(ref) =>
          scriptObjectHolder(ref)
        case ref: ObjectReference if marshaller.isJSObject(ref) =>
          val factory = scriptBasedPropertyHolderFactory()
          factory.create(ref, "", isNative = false)
        case ref: ArrayReference =>
          new ArrayPropertyHolder(ref)
        case obj: ObjectReference if marshaller.isHashtable(obj) =>
          val factory = scriptBasedPropertyHolderFactory()
          factory.create(obj, "", isNative = false)
        case obj: ObjectReference =>
          new ArbitraryObjectPropertyHolder(obj)
      }
    })
  }

  private def scriptBasedPropertyHolderFactory()(implicit threadReference: ThreadReference): ScriptBasedPropertyHolderFactory = {
    maybeScriptBasedPropertyHolderFactory match {
      case Some(f) => f
      case None =>
        val codeEvalFun: (String) => Value = src => codeEval.eval(null, null, src, Lifecycle.Session)
        val funExec: (Value, Seq[Any]) => Value = (fun, args) => {
          foundWantedTypes.get(NIR_ScriptRuntime) match {
            case Some(ct) =>
              val invoker = Invokers.shared.getStatic(ct)
              // Object apply(ScriptFunction target, Object self, Object... args) {
              invoker.apply(fun, null, args.toArray)

            case None => throw new RuntimeException("ScriptRuntime wasn't found")
          }
        }
        val f = new ScriptBasedPropertyHolderFactory(codeEvalFun, funExec)
        maybeScriptBasedPropertyHolderFactory = Some(f)
        f
    }
  }

  private def accessorToDataForLocals(objectId: ObjectId)(prop: (String, ObjectPropertyDescriptor)): (String, ObjectPropertyDescriptor) = {
    if (objectId.id.startsWith(localScopeObjectIdPrefix) && prop._2.descriptorType == PropertyDescriptorType.Accessor) {
      val desc = prop._2
      // Yeah, getting to the getter ID is ugly, but it must work since we know we have an accessor property.
      val getterId = desc.getter.get.asInstanceOf[ComplexNode].objectId
      evaluateOnStackFrame("$top", "fun.call(owner)", Map("fun" -> getterId, "owner" -> objectId)) match {
        case Success(vn) =>
          val newDescriptor = desc.copy(descriptorType = PropertyDescriptorType.Data,
            getter = None,
            setter = None,
            value = Some(vn),
            isWritable = desc.setter.isDefined
          )
          (prop._1, newDescriptor)

        case Failure(t) =>
          log.error(s"Failed to invoke the getter for ${prop._1} on $objectId", t)
          prop
      }
    } else prop
  }

}
