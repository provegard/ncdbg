package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.{IdGenerator, ObjectMapping}
import org.slf4s.Logging

import scala.collection.mutable

object Runtime {
  type ExecutionContextId = Int
  type ScriptId = String
  type RemoteObjectId = String
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
                            returnByValue: Option[Boolean])

  case class getProperties(objectId: String, ownProperties: Boolean)

  case class releaseObjectGroup(objectGroup: String)

  case class evaluate(expression: String, objectGroup: Option[String], contextId: Option[ExecutionContextId], returnByValue: Option[Boolean])

  case class compileScript(expression: String, sourceURL: String, persistScript: Boolean, executionContextId: Option[ExecutionContextId])

  case class runScript(scriptId: ScriptId, executionContextId: Option[ExecutionContextId])

  case class RemoteObject(`type`: String, subtype: String, className: String, description: String, value: Any, unserializableValue: String, objectId: String)

  case class GetPropertiesResult(result: Seq[PropertyDescriptor], exceptionDetails: Option[ExceptionDetails])

  case class PropertyDescriptor(name: String, value: RemoteObject, writable: Boolean = false, configurable: Boolean = false, enumerable: Boolean = false)

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
}

class Runtime extends DomainActor with Logging with ScriptEvaluateSupport {
  import Runtime._

  private var objectRegistry: ObjectRegistry = _
  private var remoteObjectConverter: RemoteObjectConverter = _
  private val compiledScriptIdGenerator = new IdGenerator("compscr")

  override protected def scriptHostReceived(): Unit = {
    objectRegistry = scriptHost.objectRegistry
    remoteObjectConverter = new RemoteObjectConverter()
  }

  protected def toRemoteObject(node: ValueNode, byValue: Boolean): RemoteObject =
    remoteObjectConverter.toRemoteObject(node, byValue = byValue)

  override protected def handle: PartialFunction[AnyRef, Any] = {
    case Runtime.getProperties(strObjectId, _) =>
      // Deserialize JSON object ID (serialized in RemoteObjectConverter)
      val objectId = ObjectId.fromString(strObjectId)
      objectRegistry.objectById(objectId) match {
        case Some(value) =>
          val propDescs = value.entries.map(e => PropertyDescriptor(e._1, toRemoteObject(e._2.resolve(), byValue = false)))
          GetPropertiesResult(propDescs, None)
        case None =>
          val exceptionDetails = ExceptionDetails(1, s"Error: Unknown object ID: '$strObjectId'", 0, 1, None)
          GetPropertiesResult(Seq.empty, Some(exceptionDetails))
      }

    case Domain.enable =>
      emitEvent("Runtime.executionContextCreated",
        ExecutionContextCreatedEventParams(ExecutionContextDescription(StaticExecutionContextId, "top", "top", null)))

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

    case Runtime.callFunctionOn(strObjectId, functionDeclaration, arguments, maybeSilent, maybeReturnByValue) =>
      // TODO: See Debugger.evaluateOnCallFrame - need to have a common impl
      val actualReturnByValue = maybeReturnByValue.getOrElse(false)
      val reportException = !maybeSilent.getOrElse(false)

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
      val evalResult = evaluate(scriptHost, "$top", expression, namedObjects, reportException, actualReturnByValue)
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