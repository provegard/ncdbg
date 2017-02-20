package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.{IdGenerator, ObjectMapping}
import org.slf4s.Logging

import scala.util.{Failure, Success}

object Runtime {
  type ExecutionContextId = Int
  type ScriptId = String
  type RemoteObjectId = String
  type Timestamp = Long

  /**
    * Allowed values: Infinity, NaN, -Infinity, -0.
    */
  type UnserializableValue = String

  val StaticExecutionContextId = 1 //TODO: When do we vary this?

  case object runIfWaitingForDebugger

  case class releaseObject(objectId: RemoteObjectId)

  /**
    * Used when the user interacts with an object in the DevTools console, to list possible completions.
    */
  case class callFunctionOn(objectId: RemoteObjectId, functionDeclaration: String, arguments: Seq[CallArgument], silent: Option[Boolean],
                            returnByValue: Option[Boolean], generatePreview: Option[Boolean])

  case class getProperties(objectId: String, ownProperties: Option[Boolean], accessorPropertiesOnly: Option[Boolean], generatePreview: Option[Boolean])

  case class releaseObjectGroup(objectGroup: String)

  case class evaluate(expression: String, objectGroup: Option[String], contextId: Option[ExecutionContextId], returnByValue: Option[Boolean])

  case class compileScript(expression: String, sourceURL: String, persistScript: Boolean, executionContextId: Option[ExecutionContextId])

  case class runScript(scriptId: ScriptId, executionContextId: Option[ExecutionContextId])

  /**
    * Represents a value that from the perspective of Chrome Dev Tools is remote, i.e. resides in the host.
    *
    * @param `type` Object type. Allowed values: object, function, undefined, string, number, boolean, symbol.
    * @param subtype Object subtype hint. Specified for object type values only. Allowed values: array, null, node, regexp,
    *                date, map, set, iterator, generator, error, proxy, promise, typedarray.
    * @param className Object class (constructor) name. Specified for object type values only.
    * @param description String representation of the object.
    * @param value Remote object value in case of primitive values or JSON values (if it was requested).
    * @param unserializableValue Primitive value which can not be JSON-stringified does not have value, but gets this property.
    * @param objectId Unique object identifier (for non-primitive values).
    * @param preview Preview containing abbreviated property values. Specified for object type values only.
    */
  case class RemoteObject(`type`: String, subtype: Option[String],
                          className: Option[String], description: Option[String], value: Option[Any], unserializableValue: Option[String], objectId: Option[String],
                          preview: Option[ObjectPreview] = None) {

    def emptyPreview = {
      val desc = description.getOrElse(value match {
        case Some(x) if x == null => "null"
        case Some(x) => x.toString
        case None if `type` == "undefined" => "undefined"
        case None => ""
      })
      ObjectPreview(`type`, desc, overflow = false, subtype, Seq.empty)
    }
  }

  case class ObjectPreview(`type`: String, description: String, overflow: Boolean, subtype: Option[String], properties: Seq[PropertyPreview])

  case class PropertyPreview(name: String, `type`: String, value: String, subtype: Option[String]) // subPreview

  case class GetPropertiesResult(result: Seq[PropertyDescriptor], exceptionDetails: Option[ExceptionDetails])

  // TODO: wasThrown
  case class PropertyDescriptor(name: String, writable: Boolean, configurable: Boolean, enumerable: Boolean,
                                isOwn: Boolean,
                                value: Option[RemoteObject],
                                get: Option[RemoteObject], set: Option[RemoteObject])

  case class ExecutionContextCreatedEventParams(context: ExecutionContextDescription)

  case class ExecutionContextDescription(id: ExecutionContextId, origin: String, name: String, auxData: AnyRef)

  case class EvaluateResult(result: RemoteObject)
  case class RunScriptResult(result: RemoteObject)
  case class CallFunctionOnResult(result: RemoteObject, exceptionDetails: Option[ExceptionDetails])

  case class CompileScriptResult(scriptId: ScriptId)

  case class ExceptionDetails(exceptionId: Int, text: String, lineNumber: Int, columnNumber: Int, url: Option[String], scriptId: Option[ScriptId] = None, executionContextId: ExecutionContextId = StaticExecutionContextId)

  /**
    * One of the properties is set, or none for 'undefined'.
    */
  case class CallArgument(value: Option[Any], unserializableValue: Option[UnserializableValue], objectId: Option[RemoteObjectId])

  object RemoteObject extends RemoteObjectBuilder

  object PropertyDescriptor extends PropertyDescriptorBuilder

  case class ConsoleAPICalledEventParams(`type`: String, args: Seq[RemoteObject], executionContextId: ExecutionContextId, timestamp: Timestamp)
}

class Runtime extends DomainActor with Logging with ScriptEvaluateSupport with RemoteObjectConversionSupport {
  import Runtime._

  private val compiledScriptIdGenerator = new IdGenerator("compscr")

  override protected def handle: PartialFunction[AnyRef, Any] = {
    case Runtime.getProperties(strObjectId, ownProperties, accessorPropertiesOnly, maybeGeneratePreview) =>

      log.debug(s"Runtime.getProperties: objectId = $strObjectId, ownProperties = $ownProperties, accessorPropertiesOnly = $accessorPropertiesOnly")

      val generatePreview = maybeGeneratePreview.getOrElse(false)
      implicit val remoteObjectConverter = createRemoteObjectConverter(generatePreview, byValue = false)

      // Deserialize JSON object ID (serialized in RemoteObjectConverter)
      val objectId = ObjectId.fromString(strObjectId)
      tryHostCall(_.getObjectProperties(objectId, ownProperties.getOrElse(false), accessorPropertiesOnly.getOrElse(false))) match {
        case Success(props) =>
          val propDescs = props.map((PropertyDescriptor.from _).tupled).toSeq
          GetPropertiesResult(propDescs, None)

        case Failure(t) =>
          val exceptionDetails = ExceptionDetails(1, s"Error: '${t.getMessage}' for object '$strObjectId'", 0, 1, None)
          GetPropertiesResult(Seq.empty, Some(exceptionDetails))
      }

    case Domain.enable =>
      emitEvent("Runtime.executionContextCreated",
        ExecutionContextCreatedEventParams(ExecutionContextDescription(StaticExecutionContextId, "top", "top", null)))

      emitEvent("Runtime.consoleAPICalled",
        ConsoleAPICalledEventParams("log", Seq(RemoteObject.forString("Greetings from ncdbg!")), StaticExecutionContextId, System.currentTimeMillis()))

    case Runtime.releaseObjectGroup(grp) =>
      log.debug(s"Request to release object group '$grp'")

    case Runtime.evaluate(expr, _, _, _) =>
      EvaluateResult(RemoteObject.forString("TODO: Implement Runtime.evaluate"))

    case Runtime.compileScript(expr, url, persist, _) =>
      log.debug(s"Request to compile script '$expr' with URL $url and persist = $persist")
      // In my testing, this method must be implemented for console evaluation to work properly, but Chrome never
      // calls runScript to evaluate the script. So for now we just return a dummy script ID.
      CompileScriptResult(compiledScriptIdGenerator.next)

    case Runtime.runScript(scriptId, _) =>
      log.debug(s"Request to run script with ID $scriptId")
      RunScriptResult(RemoteObject.forString("TODO: Implement Runtime.runScript"))

    case Runtime.callFunctionOn(strObjectId, functionDeclaration, arguments, maybeSilent, maybeReturnByValue, maybeGeneratePreview) =>
      // TODO: See Debugger.evaluateOnCallFrame - need to have a common impl
      val actualReturnByValue = maybeReturnByValue.getOrElse(false)
      val reportException = !maybeSilent.getOrElse(false)
      val generatePreview = maybeGeneratePreview.getOrElse(false)

      implicit val remoteObjectConverter = createRemoteObjectConverter(generatePreview, actualReturnByValue)

      val objectIdNameGenerator = new IdGenerator("__obj_")

      var namedObjects = Map[String, ObjectId]()
      def useNamedObject(objectId: ObjectId): String = {
        val name = objectIdNameGenerator.next
        namedObjects += name -> objectId
        name
      }

      val targetName = useNamedObject(ObjectId.fromString(strObjectId))

      val argsArrayString = serializeArgumentValues(arguments, useNamedObject).mkString("[", ",", "]")
      val expression = s"($functionDeclaration).apply($targetName,$argsArrayString)"

      // TODO: Stack frame ID should be something else here, to avoid the use of magic strings
      val evalResult = evaluate(scriptHost, "$top", expression, namedObjects, reportException)
      CallFunctionOnResult(evalResult.result, evalResult.exceptionDetails)

    case Runtime.runIfWaitingForDebugger =>
      log.debug("Request to run if waiting for debugger")

    case Runtime.releaseObject(objectId) =>
      log.debug(s"Request to release object with ID $objectId")
  }

  /**
    * Serialize arguments to JSON so that they can be embedded in a script.
    */
  private def serializeArgumentValues(arguments: Seq[CallArgument], useNamedObject: (ObjectId) => String): Seq[String] = {
    arguments.map { arg =>
      (arg.value, arg.unserializableValue, arg.objectId) match {
        case (Some(value), None, None) =>
          ObjectMapping.toJson(value)
        case (None, Some(unserializableValue), None) => unserializableValue
        case (None, None, Some(strObjectId)) =>
          // Obtain a name for the object with the given ID
          val objectId = ObjectId.fromString(strObjectId)
          useNamedObject(objectId)
        case (None, None, None) => "undefined"
        case _ =>
          // TODO: How can we differ between null and undefined?
          "null"
      }
    }
  }
}