package com.programmaticallyspeaking.ncd.chrome.domains

import java.nio.charset.StandardCharsets

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.infra.{BuildProperties, Hasher, IdGenerator, ObjectMapping}
import com.programmaticallyspeaking.ncd.transpile.{CachingES5Transpiler, ClosureBasedES5Transpiler}
import org.slf4s.Logging

import scala.collection.mutable
import scala.util.{Failure, Success}

object Runtime {
  type ExecutionContextId = Int
  type ScriptId = String
  type RemoteObjectId = String
  type Timestamp = Long

  object Timestamp {
    def now: Timestamp = System.currentTimeMillis()
  }

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
  case class callFunctionOn(objectId: RemoteObjectId, functionDeclaration: String, arguments: Seq[CallArgument],
                            returnByValue: Option[Boolean], generatePreview: Option[Boolean])

  case class getProperties(objectId: String, ownProperties: Option[Boolean], accessorPropertiesOnly: Option[Boolean], generatePreview: Option[Boolean])

  case class releaseObjectGroup(objectGroup: String)

  case class evaluate(expression: String, objectGroup: Option[String], contextId: Option[ExecutionContextId],
                      returnByValue: Option[Boolean], generatePreview: Option[Boolean])

  case class compileScript(expression: String, sourceURL: String, persistScript: Boolean, executionContextId: Option[ExecutionContextId])

  case class runScript(scriptId: ScriptId, executionContextId: Option[ExecutionContextId],
                       returnByValue: Boolean, generatePreview: Boolean)

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
      ObjectPreview(`type`, desc, overflow = false, subtype, Seq.empty, Seq.empty)
    }
  }

  case class ObjectPreview(`type`: String, description: String, overflow: Boolean, subtype: Option[String], properties: Seq[PropertyPreview], entries: Seq[EntryPreview])

  case class PropertyPreview(name: String, `type`: String, value: String, subtype: Option[String]) // subPreview

  // For a Map, both key and value are used. For a Set, only value is used.
  case class EntryPreview(key: Option[ObjectPreview], value: ObjectPreview)

  case class GetPropertiesResult(result: Seq[PropertyDescriptor], exceptionDetails: Option[ExceptionDetails], internalProperties: Seq[InternalPropertyDescriptor])

  // TODO: wasThrown
  case class PropertyDescriptor(name: String, writable: Boolean, configurable: Boolean, enumerable: Boolean,
                                isOwn: Boolean,
                                value: Option[RemoteObject],
                                get: Option[RemoteObject], set: Option[RemoteObject])

  case class InternalPropertyDescriptor(name: String, value: Option[RemoteObject])

  case class ExecutionContextCreatedEventParams(context: ExecutionContextDescription)

  case class ExecutionContextDescription(id: ExecutionContextId, origin: String, name: String, auxData: AnyRef)

  case class EvaluateResult(result: RemoteObject, exceptionDetails: Option[ExceptionDetails])
  case class RunScriptResult(result: RemoteObject, exceptionDetails: Option[ExceptionDetails])
  case class CallFunctionOnResult(result: RemoteObject, exceptionDetails: Option[ExceptionDetails])

  case class CompileScriptResult(scriptId: ScriptId, exceptionDetails: Option[ExceptionDetails])

  case class ExceptionDetails(exceptionId: Int, text: String, lineNumber: Int, columnNumber: Int, url: Option[String], scriptId: Option[ScriptId] = None,
                              exception: Option[RemoteObject],
                              executionContextId: ExecutionContextId = StaticExecutionContextId)

  object ExceptionDetails {
    def fromErrorValue(err: ErrorValue, exceptionId: Int)(implicit remoteObjectConverter: RemoteObjectConverter): ExceptionDetails = {
      val data = err.data
      val exception = remoteObjectConverter.toRemoteObject(err)
      // Note that Chrome wants line numbers to be 0-based
      // text=Uncaught mimics Chrome
      ExceptionDetails(exceptionId, "Uncaught", data.lineNumberBase1 - 1, data.columnNumberBase0, Some(data.url), exception = Some(exception))
    }
  }

  /**
    * One of the properties is set, or none for 'undefined'.
    */
  case class CallArgument(value: Option[Any], unserializableValue: Option[UnserializableValue], objectId: Option[RemoteObjectId])

  object RemoteObject extends RemoteObjectBuilder

  object PropertyDescriptor extends PropertyDescriptorBuilder

  object InternalPropertyDescriptor extends InternalPropertyDescriptorBuilder

  case class ConsoleAPICalledEventParams(`type`: String, args: Seq[RemoteObject], executionContextId: ExecutionContextId, timestamp: Timestamp)

  case class CallFrame(functionName: String, scriptId: ScriptId, url: String, lineNumber: Int, columnNumber: Option[Int])

  case class ExceptionThrownEventParams(timestamp: Timestamp, exceptionDetails: ExceptionDetails)

  /**
    * Console message sent if we detect Visual Studio Code. The Loaded Scripts pane may be empty, and it seems
    * like a good idea to inform the user that there is a workaround.
    */
  val LoadedScriptsInfo = "It looks like you are using Visual Studio Code. If the Loaded Scripts pane is empty, please read: https://github.com/provegard/ncdbg/blob/master/docs/VSCode.md#activate-the-node-debug-extension"
}

class Runtime(scriptHost: ScriptHost) extends DomainActor(scriptHost) with Logging with ScriptEvaluateSupport with RemoteObjectConversionSupport with TranspileSupport {

  import Runtime._

  private implicit val host = scriptHost

  private val callFunctionOnCache = mutable.Map[String, Runtime.RemoteObject]()

  private def clearCallFunctionOnCache(): Unit = {
    callFunctionOnCache.clear()
  }

  private def mapInternalProperties(props: Seq[(String, ObjectPropertyDescriptor)]) = {
    // It seems as if internal properties never have a preview.
    implicit val remoteObjectConverter = createRemoteObjectConverter(generatePreview = false, byValue = false)
    props.map((InternalPropertyDescriptor.from _).tupled).flatten
  }

  private def mapProperties(props: Seq[(String, ObjectPropertyDescriptor)], generatePreview: Boolean) = {
    implicit val remoteObjectConverter = createRemoteObjectConverter(generatePreview, byValue = false)
    props.map((PropertyDescriptor.from _).tupled)
  }

  private def safeArgs(arguments: Seq[CallArgument]): Seq[CallArgument] = Option(arguments).getOrElse(Seq.empty)

  private def callFunctionOnCacheKey(strObjectId: RemoteObjectId, functionDeclaration: String, arguments: Seq[CallArgument], returnByValue: Boolean, generatePreview: Boolean) = {
    val parts = Seq(strObjectId, functionDeclaration, returnByValue, generatePreview) ++ safeArgs(arguments).map(_.toString)
    Hasher.md5(parts.mkString("|").getBytes(StandardCharsets.UTF_8))
  }

  private def shouldCacheCallFunctionOn(functionDeclaration: String) = {
    // Ugly hack :(
    // There's no good way of knowing if a function is side-effect free, so we go on known
    // functions for now.
    functionDeclaration.contains("packRanges") ||
      functionDeclaration.contains("toStringForClipboard") ||
      functionDeclaration.contains("getEntries") ||
      functionDeclaration.contains("getCompletions")
  }

  // Translate certain Nashorn errors into Chrome errors that DevTools understands.
  // "Expected ... but found eof" => "SyntaxError: Unexpected end of input"
  // "Missing close quote"        => "SyntaxError: Unterminated template literal"
  private def translateExceptionForDevTools(expr: String, exceptionDetails: ExceptionDetails): ExceptionDetails = {
    val desc = exceptionDetails.exception.flatMap(_.description).getOrElse("")
    if (desc.contains("but found eof")) {
      exceptionDetails.copy(exception = Some(errorObject("SyntaxError", "Unexpected end of input")))
    } else if (desc.contains("Missing close quote") && expr.contains('`')) {
      // Java 9, not foolproof though (false positive for e.g. "`foo`+'bar")
      exceptionDetails.copy(exception = Some(errorObject("SyntaxError", "Unterminated template literal")))
    } else {
      exceptionDetails
    }
  }

  private def firstNonEmptyLine(s: String): String = {
    val lines = s.split("\r?\n")
    val nonEmptyTail = lines.dropWhile(_.trim == "")
    nonEmptyTail.headOption match {
      case Some(line) =>
        val moreThanOne = nonEmptyTail.length > 1
        if (moreThanOne) line + "..." else line
      case None => ""
    }
  }

  override protected def handle: PartialFunction[AnyRef, Any] = {
    case Runtime.getProperties(strObjectId, ownProperties, accessorPropertiesOnly, maybeGeneratePreview) =>

      val generatePreview = maybeGeneratePreview.getOrElse(false)

      log.debug(s"Runtime.getProperties: objectId = $strObjectId, ownProperties = ${ownProperties.getOrElse(false)}, accessorPropertiesOnly = ${accessorPropertiesOnly.getOrElse(false)}, generatePreview = $generatePreview")

      // Deserialize JSON object ID (serialized in RemoteObjectConverter)
      val objectId = ObjectId.fromString(strObjectId)
      tryHostCall(_.getObjectProperties(objectId, ownProperties.getOrElse(false), accessorPropertiesOnly.getOrElse(false))) match {
        case Success(props) =>
          def isInternal(prop: (String, ObjectPropertyDescriptor)) = ObjectPropertyDescriptor.isInternal(prop._1)
          val grouped = props.groupBy(isInternal)
          val internal = grouped.getOrElse(true, Seq.empty)
          val external = grouped.getOrElse(false, Seq.empty)
          GetPropertiesResult(mapProperties(external, generatePreview), None, mapInternalProperties(internal))

        case Failure(t) =>
          val exceptionDetails = ExceptionDetails(1, s"Error: '${t.getMessage}' for object '$strObjectId'", 0, 1, None, exception = None)
          GetPropertiesResult(Seq.empty, Some(exceptionDetails), Seq.empty)
      }

    case Domain.enable =>
      emitEvent("Runtime.executionContextCreated",
        ExecutionContextCreatedEventParams(ExecutionContextDescription(StaticExecutionContextId, "top", "top", null)))

      consoleLog(s"Greetings from NCDbg version ${BuildProperties.version}!")
      val warnings = scriptHost.warnings
      warnings.foreach(consoleWarn)

    case Runtime.releaseObjectGroup(grp) =>
      log.debug(s"Request to release object group '$grp'")

    case Runtime.evaluate(expr, _, _, maybeReturnByValue, maybeGeneratePreview) =>
      if (expr == "navigator.userAgent") {
        // VS Code (Debugger for Chrome) wants to know

        // Inform about the Loaded scripts problem
        consoleLog(LoadedScriptsInfo)

        EvaluateResult(RemoteObject.forString(s"NCDbg version ${BuildProperties.version}"), None)
      } else {
        // Arbitrary script execution may affect objects involved in the callFunctionOn cache, so clear.
        clearCallFunctionOnCache()

        // Runtime.evaluate evaluates on the global object. Calling with null as 'this' results in exactly that.
        val script = s"(function(){return ($expr);}).call(null);"

        // TODO: Debugger.evaluateOnCallFrame + Runtime.callFunctionOn, duplicate code
        val actualReturnByValue = maybeReturnByValue.getOrElse(false)
        val generatePreview = maybeGeneratePreview.getOrElse(false)

        implicit val remoteObjectConverter = createRemoteObjectConverter(generatePreview, actualReturnByValue)

        val evalResult = evaluate(scriptHost, "$top", script)
        EvaluateResult(evalResult.result, evalResult.exceptionDetails)
      }

    case Runtime.compileScript(expr, url, persist, _) =>
      def firstLine = firstNonEmptyLine(expr)
      log.info(s"Request to compile script that starts '$firstLine' with URL '$url' and persist = $persist")

      // If persist is false, then we may get None back in which case we cannot report an ID.
      scriptHost.compileScript(expr, url, persist).map(s => CompileScriptResult(s.map(_.id).orNull, None)).recover {
        case t =>
          val exceptionDetails = exceptionDetailsFromError(t, 1)
          val returnedDetails = translateExceptionForDevTools(expr, exceptionDetails)
          CompileScriptResult(null, Some(returnedDetails))
      }

    case Runtime.runScript(scriptId, _, returnByValue, generatePreview) =>
      //TODO: silent "Overrides setPauseOnException state."
      log.info(s"Request to run script with ID $scriptId")
      scriptHost.runCompiledScript(scriptId) match {
        case Success(v) =>
          val remoteObjectConverter = createRemoteObjectConverter(generatePreview, returnByValue)
          val ro = remoteObjectConverter.toRemoteObject(v)
          RunScriptResult(ro, None)

        case Failure(t) =>
          val exceptionDetails = exceptionDetailsFromError(t, 1)
          log.debug("Responding with run-script error: " + exceptionDetails.text)
          RunScriptResult(RemoteObject.undefinedValue, Some(exceptionDetails))
      }

    case Runtime.callFunctionOn(strObjectId, functionDeclaration, arguments, maybeReturnByValue, maybeGeneratePreview) =>
      // TODO: See Debugger.evaluateOnCallFrame - need to have a common impl
      val actualReturnByValue = maybeReturnByValue.getOrElse(false)
      val generatePreview = maybeGeneratePreview.getOrElse(false)

      val cacheKey = callFunctionOnCacheKey(strObjectId, functionDeclaration, arguments, actualReturnByValue, generatePreview)
      val isCacheableFunction = shouldCacheCallFunctionOn(functionDeclaration)

      if (!isCacheableFunction) {
        // The function may have side effects, so clear the cache.
        clearCallFunctionOnCache()
      }

      callFunctionOnCache.get(cacheKey) match {
        case Some(result) =>
          log.debug(s"Reusing cached result for callFunctionOn on object $strObjectId")
          CallFunctionOnResult(result, None)

        case None =>
          implicit val remoteObjectConverter = createRemoteObjectConverter(generatePreview, actualReturnByValue)

//          val namedObjects = new NamedObjects

          val thisObjectId = ObjectId.fromString(strObjectId)
//          val targetName = namedObjects.useNamedObject(ObjectId.fromString(strObjectId))

          // Transpile the code if needed.
          // Some considerations:
          // - perhaps we should transpile always? But we'd like to skip the transpilation runtime if it's not needed.
          // - perhaps we should transpile in Runtime.evaluate also?
          val maybeTranspiled = if (needsTranspile(functionDeclaration)) transpile(functionDeclaration) else functionDeclaration

//          val argsArrayString = ScriptEvaluateSupport.serializeArgumentValues(safeArgs(arguments), namedObjects).mkString("[", ",", "]")
//          val expression = s"($maybeTranspiled).apply($targetName,$argsArrayString)"
          val (wrapperFunc, objIds) = ScriptEvaluateSupport.wrapInFunction(maybeTranspiled, arguments)

          // TODO: Stack frame ID should be something else here, to avoid the use of magic strings
          val evalResult = callFunctionOn(scriptHost, "$top", Some(thisObjectId), wrapperFunc, objIds)

          if (evalResult.exceptionDetails.isEmpty && isCacheableFunction) {
            // Store in cache
            callFunctionOnCache += cacheKey -> evalResult.result
          }

          CallFunctionOnResult(evalResult.result, evalResult.exceptionDetails)
      }

    case Runtime.runIfWaitingForDebugger =>
      log.debug("Request to run if waiting for debugger")

    case Runtime.releaseObject(objectId) =>
      log.debug(s"Request to release object with ID $objectId")
  }

  override protected def handleScriptEvent: PartialFunction[ScriptEvent, Unit] = {
    case UncaughtError(ev) =>
      // TODO: What do use for exceptionId?
      implicit val remoteObjectConverter = createRemoteObjectConverter(false, false)
      emitEvent("Runtime.exceptionThrown", ExceptionThrownEventParams(Timestamp.now, ExceptionDetails.fromErrorValue(ev, 1)))

    case PrintMessage(msg) =>
      consoleLog(msg)

    case Resumed =>
      // It seems like a good idea to not keep the cache in this case.
      clearCallFunctionOnCache()
  }

  private def consoleLog(msg: String) = {
    emitEvent("Runtime.consoleAPICalled",
      ConsoleAPICalledEventParams("log", Seq(RemoteObject.forString(msg)), StaticExecutionContextId, Timestamp.now))
  }
  private def consoleWarn(msg: String) = {
    emitEvent("Runtime.consoleAPICalled",
      ConsoleAPICalledEventParams("warning", Seq(RemoteObject.forString(msg)), StaticExecutionContextId, Timestamp.now))
  }
}