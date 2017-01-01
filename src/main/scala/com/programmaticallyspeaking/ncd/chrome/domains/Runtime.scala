package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.{IdGenerator, ObjectMapping}
import org.slf4s.Logging

object Runtime {
  type ExecutionContextId = Int
  type ScriptId = String

  val StaticExecutionContextId = 1 //TODO: When do we vary this?

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

  case class CompileScriptResult(scriptId: ScriptId)

  case class ExceptionDetails(exceptionId: Int, text: String, lineNumber: Int, columnNumber: Int, url: Option[String], scriptId: Option[ScriptId] = None, executionContextId: ExecutionContextId = StaticExecutionContextId)

  object RemoteObject extends RemoteObjectBuilder
}

class Runtime extends DomainActor with Logging {
  import Runtime._

  private var objectRegistry: ObjectRegistry = _
  private var remoteObjectConverter: RemoteObjectConverter = _
  private var compiledScriptIdGenerator = new IdGenerator("compscr")

  override protected def scriptHostReceived(): Unit = {
    objectRegistry = scriptHost.objectRegistry
    remoteObjectConverter = new RemoteObjectConverter()
  }

  private def toRemoteObject(node: ValueNode, byValue: Boolean): RemoteObject =
    remoteObjectConverter.toRemoteObject(node, byValue = byValue)

  override protected def handle: PartialFunction[AnyRef, Any] = {
    case Runtime.getProperties(jsonObjectId, _) =>
      // Deserialize JSON object ID (serialized in RemoteObjectConverter)
      val objectId = ObjectMapping.fromJson[ObjectId](jsonObjectId)
      objectRegistry.objectById(objectId) match {
        case Some(value) =>
          val propDescs = value.entries.map(e => PropertyDescriptor(e._1, toRemoteObject(e._2.resolve(), byValue = false)))
          GetPropertiesResult(propDescs, None)
        case None =>
          val exceptionDetails = ExceptionDetails(1, s"Error: Unknown object ID: '$jsonObjectId'", 0, 1, None)
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
  }
}