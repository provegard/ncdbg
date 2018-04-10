package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.host.{ComplexNode, MapSetEntryNode, ObjectId, ObjectNode}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost._
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi.{ArrayReference, ObjectReference, ThreadReference, Value}
import org.slf4s.Logging

trait ObjectPropertiesSupport extends NashornScriptHost { self: NashornDebuggerHost with Logging =>
  import NashornDebuggerHost._
  import TypeConstants._
  import JDIExtensions._

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

      mappingRegistry.byId(objectId) match {
        case Some(desc: ObjectDescriptor) =>
          // Skip proto for Map/Set entry objects and for scope objects.
          val includeProto = !isScopeObject && !desc.marshalled.isInstanceOf[MapSetEntryNode]

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
      throw new IllegalStateException(s"Property extraction can only be done in a paused state (for object $objectId).")
  }

  private def createPropertyHolder(objectId: ObjectId, objectDescriptor: ObjectDescriptor, includeProto: Boolean)(implicit marshaller: Marshaller): Option[PropertyHolder] = {
    val cache = pausedData.get.propertyHolderCache
    implicit val thread = marshaller.thread

    def scriptObjectHolder(ref: ObjectReference) = {
      val factory = scriptBasedPropertyHolderFactory()
      val isScopeObject = objectId.id.startsWith(localScopeObjectIdPrefix)

      var blacklistParts = Seq(hiddenPrefixEscapedForUseInJavaScriptRegExp + ".+")
      if (isScopeObject) blacklistParts +:= "__noSuchProperty__" // see StackBuilder.scopeWithFreeVariables
      if (!includeProto) blacklistParts +:= "__proto__"
      val propBlacklistRegex = blacklistParts.map("(" + _ + ")").mkString("^", "|", "$")
      val holder = factory.create(ref, propBlacklistRegex, isNative = true, isScopeObject)
      new PropertyHolder {
        override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)] = {
          holder.properties(onlyOwn, onlyAccessors)
        }
      }
    }

    //TODO: We can use scriptObjectHolder for ScriptObjectMirror also, but when do we one of those?
    cache.getOrElseUpdate(objectId, {
      objectDescriptor.native collect {
        case ref: ObjectReference if marshaller.isScriptObject(ref) =>
          val mirror = new ScriptObjectMirror(ref)
          if (ref.isWithObject) scriptObjectHolder(mirror.getExpression()) else scriptObjectHolder(ref)
        case ref: ObjectReference if marshaller.isJSObject(ref) =>
          val factory = scriptBasedPropertyHolderFactory()
          factory.create(ref, "", isNative = false, isScopeObject = false)
        case ref: ArrayReference =>
          new ArrayPropertyHolder(ref)
        case obj: ObjectReference if marshaller.isHashtable(obj) =>
          val factory = scriptBasedPropertyHolderFactory()
          factory.create(obj, "", isNative = false, isScopeObject = false)
        case obj: ObjectReference =>
          new ArbitraryObjectPropertyHolder(obj)
        case obj: LocalObject =>
          new MapPropertyHolder(obj.values)
      }
    })
  }

  private def scriptBasedPropertyHolderFactory()(implicit threadReference: ThreadReference): ScriptBasedPropertyHolderFactory = {
    maybeScriptBasedPropertyHolderFactory match {
      case Some(f) => f
      case None =>
        // This function is only used to evaluate the property extraction function
        val codeEvalFun: (String) => Value = src => codeEval.eval(None, None, src, Lifecycle.Session)

        // It is important that the executor doesn't reuse the thread passed to scriptBasedPropertyHolderFactory
        // and used for the initial code evaluation, since that thread may be dead once the extractor function
        // is actually called.
        def functionExecutor(function: Value, arguments: Seq[Any], execThreadReference: ThreadReference): Value = {
          typeLookup(NIR_ScriptRuntime) match {
            case Some(ct) =>
              val invoker = Invokers.shared.getStatic(ct)
              // Object apply(ScriptFunction target, Object self, Object... args) {
              invoker.apply(function, null, arguments.toArray)(execThreadReference)

            case None => throw new RuntimeException("ScriptRuntime wasn't found")
          }
        }
        val f = new ScriptBasedPropertyHolderFactory(codeEvalFun, functionExecutor)
        maybeScriptBasedPropertyHolderFactory = Some(f)
        f
    }
  }

}
