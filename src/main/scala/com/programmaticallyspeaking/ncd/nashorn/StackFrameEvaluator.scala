package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{InvokeFunctionData, StackFrameImpl, hiddenPrefix, stackFrameIndexExtraProp}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi.{ObjectReference, StringReference, Value}
import org.slf4s.Logging

import scala.util.{Failure, Success, Try}

class StackFrameEvaluator(mappingRegistry: MappingRegistry, boxer: Boxer) extends Logging {

//  private def nativeValueForObjectId(objectId: ObjectId): Option[Value] = {
//    mappingRegistry.byId(objectId) match {
//      // TODO: Should we handle extras here?
//      case Some(descriptor) if descriptor.native.isDefined => Some(descriptor.native.get)
//      case Some(_) => None
//      case _ =>
//        throw new IllegalArgumentException(s"No object with ID '$objectId' was found.")
//    }
//  }
  private def nativeValueForObject(objId: ObjectId) = mappingRegistry.byId(objId).flatMap(_.native).getOrElse(throw new IllegalArgumentException("No native Value for object: " + objId))

  def evaluateOnStackFrame(pd: PausedData, stackFrameId: String, expression: String, invokeFunctionData: Option[InvokeFunctionData]): ValueNode = {
    findStackFrame(pd, stackFrameId) match {
      case Some(sf: StackFrameImpl) =>
        implicit val marshaller = pd.marshaller

        // To be able to detect changes to local variables, find the artificial local scope, if any.
        val localScopeId = sf.scopeChain.find(_.scopeType == ScopeType.Local).map(_.value).collect { case c: ComplexNode => c.objectId }
        val localScope = localScopeId.map(id => (id, nativeValueForObject(id)))

        // Evaluating code may modify any existing object, which means that we cannot keep our object properties
        // cache. There's no point trying to be smart here and only remove entries for the named objects, since the
        // code may call a function that modifies an object that we don't know about here.
        pd.objectPropertiesCache.clear()

        // By resetting change tracking before evaluating the expression, we can track changes made to local vars.
        resetChangeTracking(sf, localScope)

        val result = sf.eval(expression, invokeFunctionData)

        // Update locals that changed, if needed. It's not sufficient for the synthetic locals object to have
        // been updated, since generated Java code will access the local variables directly.
        updateChangedLocals(sf, localScope)
        result
      case _ =>
        log.warn(s"Cannot evaluate '$expression', because no stack frame found with ID $stackFrameId. Available IDs: " + pd.stackFrames.map(_.id).mkString(", "))
        throw new IllegalArgumentException(s"Failed to find a stack frame with ID $stackFrameId")
    }
  }

//  /**
//    * Finds a unique name for the locals object we pass to updateChangedLocals. Probably overkill.
//    */
//  private def localsName(objectNames: Set[String]): String = {
//    val origName = "___locals"
//    var name = origName
//    var idx = 1
//    while (objectNames.contains(name)) {
//      name = origName + idx
//      idx += 1
//    }
//    name
//  }

  private def updateChangedLocals(sf: StackFrameImpl, localScope: Option[(ObjectId, Value)])(implicit marshaller: Marshaller): Unit = {
    def stackFrameIndexForObject(id: ObjectId): Option[Int] =
      mappingRegistry.byId(id).flatMap(_.extras.get(stackFrameIndexExtraProp)).flatMap(_.as[Number].map(_.intValue()))

    def jdiStackFrameFromIndex(index: Int) = marshaller.thread.frame(index)

    localScope.foreach { case (objectId, value) =>
      stackFrameIndexForObject(objectId) match {
        case Some(stackFrameIdx) =>
          val invokeFunctionData = InvokeFunctionData(null, Seq(value))
          val funcDecl = s"function (ls) { return ls['${hiddenPrefix}changes']; }"

          // Read the changes tracked by the property setters, if any.
          val changes = sf.eval(funcDecl, Some(invokeFunctionData))

          arrayValuesFrom(changes) match {
            case Right(values) if values.nonEmpty =>

              // Get the stack frame. We cannot do that earlier due to marshalling, which causes the thread to resume.
              val jdiStackFrame = jdiStackFrameFromIndex(stackFrameIdx)
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
            case Right(_) => // empty changes, noop
            case Left(reason) =>
              log.warn(s"Failed to read changes from local scope: $reason")
          }

        case None =>
        // We didn't find a stack frame index, so not a scope object
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

  private def resetChangeTracking(sf: StackFrameImpl, localScope: Option[(ObjectId, Value)]): Unit = {
    localScope.foreach { case (objectId, value) =>
      val invokeFunctionData = InvokeFunctionData(null, Seq(value))
      val funcDecl = s"function (ls) { if(typeof ls['${hiddenPrefix}resetChanges']==='function') ls['${hiddenPrefix}resetChanges'](); }"

      sf.eval(funcDecl, Some(invokeFunctionData)) match {
        case ErrorValue(data, _, _, _) =>
          val msg = data.stackIncludingMessage.getOrElse(data.message)
          throw new RuntimeException("Failed to reset change tracking: " + msg)
        case _ =>
      }
    }
  }

  private def findStackFrame(pausedData: PausedData, id: String): Option[StackFrame] = {
    if (StackFrame.isTopId(id)) return pausedData.stackFrames.headOption
    pausedData.stackFrames.find(_.id == id)
  }

  // If the type name is something like 'int', i.e. without a dot, it's bound to be primitive. I think.
  private def typeNameLooksPrimitive(typeName: String) = typeName.indexOf('.') < 0
}
