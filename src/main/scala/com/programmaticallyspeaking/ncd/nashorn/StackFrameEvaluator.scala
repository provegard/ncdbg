package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{StackFrameImpl, hiddenPrefix, stackFrameIndexExtraProp}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi.{ObjectReference, StringReference, Value}
import org.slf4s.Logging

import scala.util.{Failure, Success, Try}

class StackFrameEvaluator(mappingRegistry: MappingRegistry, boxer: Boxer) extends Logging {

  private def nativeValueForObjectId(objectId: ObjectId): Option[Value] = {
    mappingRegistry.byId(objectId) match {
      // TODO: Should we handle extras here?
      case Some(descriptor) if descriptor.native.isDefined => Some(descriptor.native.get)
      case Some(_) => None
      case _ =>
        throw new IllegalArgumentException(s"No object with ID '$objectId' was found.")
    }
  }

  private def valuesForObjects(namedObjects: Map[String, ObjectId]): Map[String, Value] = {
    namedObjects.flatMap {
      case (name, objectId) => nativeValueForObjectId(objectId).map(name -> _).toSeq
    }
  }

  def evaluateOnStackFrame(pd: PausedData, stackFrameId: String, expression: String, namedObjects: Map[String, ObjectId]): ValueNode = {
    findStackFrame(pd, stackFrameId) match {
      case Some(sf: StackFrameImpl) =>
        implicit val marshaller = pd.marshaller

        // Get the Value instances corresponding to the named objects
        val namedValues = valuesForObjects(namedObjects)

        // To be able to detect changes to local variables, find the local scope (unless it's in namedObjects already,
        // e.g. in the setVariableValue case).
        val localScopeId = sf.scopeChain.find(_.scopeType == ScopeType.Local).map(_.value) match {
          case Some(c: ComplexNode) if !namedObjects.values.toSeq.contains(c.objectId) => Some(c.objectId)
          case _ => None
        }
        val localScopeValue = localScopeId.flatMap(nativeValueForObjectId)

        // Evaluating code may modify any existing object, which means that we cannot keep our object properties
        // cache. There's no point trying to be smart here and only remove entries for the named objects, since the
        // code may call a function that modifies an object that we don't know about here.
        pd.objectPropertiesCache.clear()

        // By resetting change tracking before evaluating the expression, we can track changes made to any
        // named objects.
        resetChangeTracking(sf, namedValues)

        val result = sf.eval(expression, namedValues)

        // Update locals that changed, if needed. It's not sufficient for the synthetic locals object to have
        // been updated, since generated Java code will access the local variables directly.
        // Add the local scope here, if any.
        val localScopeName = localsName(namedObjects.keys.toSet)
        updateChangedLocals(sf,
          namedValues ++ localScopeValue.map(localScopeName -> _),
          namedObjects ++ localScopeId.map(localScopeName -> _))

        result
      case _ =>
        log.warn(s"Cannot evaluate '$expression', because no stack frame found with ID $stackFrameId. Available IDs: " + pd.stackFrames.map(_.id).mkString(", "))
        throw new IllegalArgumentException(s"Failed to find a stack frame with ID $stackFrameId")
    }
  }

  /**
    * Finds a unique name for the locals object we pass to updateChangedLocals. Probably overkill.
    */
  private def localsName(objectNames: Set[String]): String = {
    val origName = "___locals"
    var name = origName
    var idx = 1
    while (objectNames.contains(name)) {
      name = origName + idx
      idx += 1
    }
    name
  }

  private def updateChangedLocals(sf: StackFrameImpl, namedValues: Map[String, AnyRef], namedObjects: Map[String, ObjectId])(implicit marshaller: Marshaller): Unit = {
    def jdiStackFrameForObject(id: ObjectId) =
      mappingRegistry.byId(id).flatMap(_.extras.get(stackFrameIndexExtraProp)).flatMap(_.as[Number]).map(n => marshaller.thread.frame(n.intValue()))

    // Note: namedValues is created from namedObjects, so we access namedObjects directly (not via get)
    namedValues.map(e => (e._1, e._2, namedObjects(e._1))).foreach {
      case (key, value, objectId) =>
        // Read the changes tracked by the property setters, if any.
        val changes = sf.eval(s"$key['${hiddenPrefix}changes']", Map(key -> value))
        arrayValuesFrom(changes) match {
          case Right(values) if values.nonEmpty =>

            // Get the stack frame. We cannot do that earlier due to marshalling, which causes the thread to resume.
            jdiStackFrameForObject(objectId) match {
              case Some(jdiStackFrame) =>

                values.grouped(2).collect { case (str: StringReference) :: v :: Nil => str.value() -> v }.foreach {
                  case (name, newValue) =>
                    // We have almost everything we need. Find the LocalVariable and set its value.
                    Try(Option(jdiStackFrame.visibleVariableByName(name))).map(_.foreach(localVar => {
                      // Unbox if necessary
                      val valueToSet = newValue match {
                        case objRef: ObjectReference if typeNameLooksPrimitive(localVar.typeName()) => boxer.unboxed(objRef)
                        case other => other
                      }
                      jdiStackFrame.setValue(localVar, valueToSet)
                    })) match {
                      case Success(_) =>
                        log.debug(s"Updated the value of $name for $objectId to $newValue in ${jdiStackFrame.location()}")
                      case Failure(t) =>
                        log.error(s"Failed to update the value of $name for $objectId to $newValue", t)
                    }

                }

              case None =>
                log.warn(s"Failed to find the stack frame hosting $objectId")
            }
          case Right(_) => // empty changes, noop
          case Left(reason) =>
            log.warn(s"Failed to read changes from $key: $reason")
        }
    }
  }

  private def arrayValuesFrom(vn: ValueNode)(implicit marshaller: Marshaller): Either[String, List[Value]] = {
    vn match {
      case an: ArrayNode =>
        mappingRegistry.byId(an.objectId).flatMap(_.native) match {
          case Some(objRef: ObjectReference) if marshaller.isScriptObject(objRef) =>
            val mirror = new ScriptObjectMirror(objRef)
            if (mirror.isArray) {
              val arrMirror = mirror.asArray
              Right((0 until arrMirror.length).map(arrMirror.at).toList)
            } else Left("Unexpected script object type: " + mirror.className)
          case Some(other) => Left("Not a script object (should be NativeArray): " + other)
          case None => Left("Unknown object ID: " + an.objectId)
        }
      case SimpleValue(Undefined) => Right(List.empty)
      case EmptyNode => Right(List.empty)
      case other => Left("Not a marshalled array: " + other)
    }
  }

  private def resetChangeTracking(sf: StackFrameImpl, namedValues: Map[String, AnyRef]): Unit = {
    if (namedValues.isEmpty) return
    val objectNames = namedValues.keys.mkString(",")
    val js =
      s"""[$objectNames].forEach(function (obj) {
         |  if(typeof obj['${hiddenPrefix}resetChanges']==='function') obj['${hiddenPrefix}resetChanges']();
         |});
       """.stripMargin
    sf.eval(js, namedValues) match {
      case ErrorValue(data, _, _, _) =>
        throw new RuntimeException("Failed to reset change tracking: " + data.message)
      case _ =>
    }
  }

  private def findStackFrame(pausedData: PausedData, id: String): Option[StackFrame] = {
    if (id == "$top") return pausedData.stackFrames.headOption
    pausedData.stackFrames.find(_.id == id)
  }

  // If the type name is something like 'int', i.e. without a dot, it's bound to be primitive. I think.
  private def typeNameLooksPrimitive(typeName: String) = typeName.indexOf('.') < 0
}
