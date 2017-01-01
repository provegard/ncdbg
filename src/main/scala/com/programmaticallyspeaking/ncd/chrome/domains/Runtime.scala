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

  object RemoteObject {

    def forFunction(name: String, source: String, objectId: String) = {
      val desc = Option(source).getOrElse(s"function $name() { [unknown] }")
      RemoteObject("function", null, "Function", desc, null, null, validObjectId(objectId))
    }

    private def validObjectId(objectId: String): String = {
      require(objectId != null && objectId.nonEmpty, "object ID is mandatory")
      objectId
    }

    def forObject(objectId: String) = {
      RemoteObject("object", null, "Object", "Object", null, null, validObjectId(objectId))
    }

    def forObject(value: Map[String, Any]) = {
      // Note: I don't know if object ID should be omitted here. The protocol doesn't say.
      RemoteObject("object", null, "Object", "Object", value, null, null)
    }

    def forArray(length: Int, objectId: String) = {
      require(length >= 0, "array length must be non-negative")
      val desc = s"Array[$length]"
      RemoteObject("object", "array", "Array", desc, null, null, validObjectId(objectId))
    }

    def forArray(items: Seq[_]) = {
      val desc = s"Array[${items.length}]"
      // Note: I don't know if object ID should be omitted here. The protocol doesn't say.
      RemoteObject("object", "array", "Array", desc, items, null, null)
    }

    def forString(s: String) =
      Option(s).map(RemoteObject("string", null, null, null, _, null, null)).getOrElse(nullValue)


    def forNumber(number: Double): RemoteObject = {
      if (number.isNaN || number.isInfinity) {
        return RemoteObject("number", null, null, number.toString, null, number.toString, null)
      }
      if (number == 0 && (1 / number) < 0) {
        // Negative 0
        return RemoteObject("number", null, null, "-0", null, "-0", null)
      }

      RemoteObject("number", null, null, number.toString, number, null, null)
    }

    def forNumber(number: Int): RemoteObject = RemoteObject("number", null, null, number.toString, number, null, null)
    def forNumber(number: Long): RemoteObject = RemoteObject("number", null, null, number.toString, number, null, null)

    val trueValue = RemoteObject("boolean", null, null, null, true, null, null)
    val falseValue = RemoteObject("boolean", null, null, null, false, null, null)
    val nullValue = RemoteObject("object", "null", null, null, null, null, null)
    val undefinedValue = RemoteObject("undefined", null, null, null, null, null, null)

    def forError(name: String, message: String, stack: Option[String], objectId: String) =
      RemoteObject("object", "error", name, stack.getOrElse(s"$name: $message"), null, null, validObjectId(objectId))

    def forDate(stringRepresentation: String, objectId: String) =
      RemoteObject("object", "date", "Date", stringRepresentation, null, null, validObjectId(objectId))

//        { type: 'object',
//                subtype: 'regexp',
//                className: 'RegExp',
//                description: '/.*/g',
//                objectId: '{"injectedScriptId":2,"id":7}' }
  }
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