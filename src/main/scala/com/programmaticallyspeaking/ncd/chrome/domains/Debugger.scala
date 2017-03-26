package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.host.{Scope => HostScope, _}
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import org.slf4s.Logging

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object Debugger {
  type CallFrameId = String

  case object stepOver
  case object stepInto
  case object stepOut

  case object resume

  case class evaluateOnCallFrame(callFrameId: CallFrameId, expression: String, silent: Option[Boolean], returnByValue: Option[Boolean], generatePreview: Option[Boolean])

  case class EvaluateOnCallFrameResult(result: Runtime.RemoteObject, exceptionDetails: Option[Runtime.ExceptionDetails] = None)

  case class ScriptParsedEventParams(scriptId: String, url: String, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int, executionContextId: Int, hash: String,
                                     hasSourceURL: Boolean)

  case class setBreakpointsActive(active: Boolean)

  case class setVariableValue(scopeNumber: Int, variableName: String, newValue: Runtime.CallArgument, callFrameId: CallFrameId)

  /** Defines pause on exceptions state. Can be set to stop on all exceptions, uncaught exceptions or no exceptions.
    * Initial pause on exceptions state is none.
    *
    * @param state Pause on exceptions mode. Allowed values: none, uncaught, all.
    */
  case class setPauseOnExceptions(state: String)

  /** Returns possible locations for breakpoint. scriptId in start and end range locations should be the same.
    *
    * Note: Not part of stable API!?
    *
    * @param start Start of range to search possible breakpoint locations in.
    * @param end End of range to search possible breakpoint locations in (excluding).
    *            When not specifed, end of scripts is used as end of range.
    * @see https://chromedevtools.github.io/debugger-protocol-viewer/tot/Debugger/#method-getPossibleBreakpoints
    */
  case class getPossibleBreakpoints(start: Location, end: Option[Location])

  object ScriptParsedEventParams {
    def apply(script: Script): ScriptParsedEventParams = new ScriptParsedEventParams(script.id,
      script.uri,
      0, // offset
      0, // offset
      Math.max(script.lineCount - 1, 0), // offset
      endColumn = script.lastLineLength, // NOT an offset: "Length of the last line of the script" according to the docs
      Runtime.StaticExecutionContextId,
      script.contentsHash(),
      true // we have an URL for all scripts
    )
  }

  case class getScriptSource(scriptId: String)

  case class GetScriptSourceResult(scriptSource: String)

  case class GetPossibleBreakpointsResult(locations: Seq[Location])

  case class setBreakpointByUrl(lineNumber: Int, url: String, columnNumber: Int, condition: String)

  case class removeBreakpoint(breakpointId: String)

  case class SetBreakpointByUrlResult(breakpointId: String, locations: Seq[Location])

  case class Location(scriptId: String, lineNumber: Int, columnNumber: Int)

  case class CallFrame(callFrameId: CallFrameId, location: Location, scopeChain: Seq[Scope], `this`: RemoteObject, functionName: String, returnValue: RemoteObject = null)

  /**
    *
    * @param `type` Scope type. Allowed values: global, local, with, closure, catch, block, script.
    * @param `object` Object representing the scope. For global and with scopes it represents the actual object; for the rest of
    *                 the scopes, it is artificial transient object enumerating scope variables as its properties.
    */
  case class Scope(`type`: String, `object`: RemoteObject)

  case class PausedEventParams(callFrames: Seq[CallFrame], reason: String, hitBreakpoints: Seq[String])

  private[Debugger] case class EmitScriptParsed(script: Script)
}

class Debugger extends DomainActor with Logging with ScriptEvaluateSupport with RemoteObjectConversionSupport {
  import Debugger._
  import com.programmaticallyspeaking.ncd.infra.BetterOption._

  override def postStop(): Unit = try {
    // Tell the ScriptHost to reset, so that we don't leave it paused
    Option(scriptHost).foreach(_.reset())
  } finally super.postStop()

  /** Key = script ID
    * Value = contents hash
    */
  private val emittedScripts = mutable.Map[String, String]()

  private var lastCallFrameList: Option[Seq[CallFrame]] = None

  private def emitScriptParsedEvent(script: Script) = {
    val hash = script.contentsHash()
    if (emittedScripts.getOrElse(script.id, "") == hash) {
      log.trace(s"Won't re-emit scriptParsed event for script with ID '${script.id}' and same hash ($hash) as before.")
    } else {
      emittedScripts += script.id -> hash
      emitEvent("Debugger.scriptParsed", ScriptParsedEventParams(script))
    }
  }

  override protected def customReceive: Receive = {
    case EmitScriptParsed(script) => emitScriptParsedEvent(script)
  }

  override def handle = {
    case Domain.enable =>
      log.info("Enabling debugging, sending all parsed scripts to the client.")
      scriptHost.scripts.foreach(emitScriptParsedEvent)

      scriptHost.pauseOnBreakpoints()

    case Debugger.getScriptSource(scriptId) =>
      log.debug(s"Requested script source for script with ID $scriptId")
      scriptHost.scriptById(scriptId) match {
        case Some(script) =>
          GetScriptSourceResult(script.contents)
        case None =>
          throw new IllegalArgumentException("Unknown script ID: " + scriptId)
      }

    case Debugger.setBreakpointByUrl(lineNumberBase0, url, _, _) =>
      val lineNumberBase1 = lineNumberBase0 + 1
      scriptHost.setBreakpoint(url, lineNumberBase1) match {
        case Some(bp) =>
          SetBreakpointByUrlResult(bp.breakpointId, Seq(Location(bp.scriptId, bp.lineNumberBase1 - 1, 0)))
        case None =>
          log.warn(s"Cannot identify breakpoint at $url:$lineNumberBase1")
          SetBreakpointByUrlResult(null, Seq.empty)
      }


    case Debugger.resume =>
      lastCallFrameList = None
      scriptHost.resume()

    case Debugger.removeBreakpoint(id) =>
      scriptHost.removeBreakpointById(id)

    case Debugger.stepInto =>
      lastCallFrameList = None
      scriptHost.step(StepInto)

    case Debugger.stepOver =>
      lastCallFrameList = None
      scriptHost.step(StepOver)

    case Debugger.stepOut =>
      lastCallFrameList = None
      scriptHost.step(StepOut)

    case Debugger.setPauseOnExceptions(state) =>
      val pauseType = state match {
        case "none" => ExceptionPauseType.None
        case "uncaught" => ExceptionPauseType.Uncaught
        case "all" => ExceptionPauseType.All
        case _ => throw new IllegalArgumentException("Unknown pause state: " + state)
      }
      scriptHost.pauseOnExceptions(pauseType)

    case Debugger.evaluateOnCallFrame(callFrameId, expression, maybeSilent, maybeReturnByValue, maybeGeneratePreview) =>
      // TODO: "In silent mode exceptions thrown during evaluation are not reported and do not pause execution. Overrides setPauseOnException state."
      // TODO: -- Obey setPauseOnException - and what about pausing??

      // The protocol says this is optional, but doesn't specify the default value. False is just a guess.
      val actualReturnByValue = maybeReturnByValue.getOrElse(false)
      val reportException = !maybeSilent.getOrElse(false)
      val generatePreview = maybeGeneratePreview.getOrElse(false)

      implicit val remoteObjectConverter = createRemoteObjectConverter(generatePreview, actualReturnByValue)

      val evalResult = evaluate(scriptHost, callFrameId, expression, Map.empty, reportException)
      EvaluateOnCallFrameResult(evalResult.result, evalResult.exceptionDetails)

    case Debugger.setBreakpointsActive(active) =>
      if (active) scriptHost.pauseOnBreakpoints()
      else scriptHost.ignoreBreakpoints()

    case Debugger.getPossibleBreakpoints(start, maybeEnd) =>
      // TODO: Unit test if works
      val locations = scriptHost.getBreakpointLineNumbers(start.scriptId, start.lineNumber + 1, maybeEnd.map(_.lineNumber + 1))
        .map(line1Based => Location(start.scriptId, line1Based - 1, 0))
      GetPossibleBreakpointsResult(locations)

    case Debugger.setVariableValue(scopeNum, varName, newValue, callFrameId) =>
      val scopeObjectId = for {
        callFrames <- lastCallFrameList.toEither("Debugger hasn't stopped on a breakpoint")
        callFrame <- callFrames.find(_.callFrameId == callFrameId)
          .toEither(s"No call frame with ID $callFrameId found (available: ${callFrames.map(_.callFrameId).mkString(", ")})")
        scope <- callFrame.scopeChain.lift(scopeNum).toEither(s"Scope no. $scopeNum not found (count: ${callFrame.scopeChain.size})")
        scopeObjectId <- scope.`object`.objectId.toEither(s"Missing scope object ID")
      } yield scopeObjectId

      scopeObjectId match {
        case Right(strObjectId) =>

          // TODO: Big-time copy-paste from Runtime.callFunctionOn
          implicit val remoteObjectConverter = createRemoteObjectConverter(false, false)

          val objectIdNameGenerator = new IdGenerator("__obj_")

          var namedObjects = Map[String, ObjectId]()
          def useNamedObject(objectId: ObjectId): String = {
            val name = objectIdNameGenerator.next
            namedObjects += name -> objectId
            name
          }

          val scopeName = useNamedObject(ObjectId.fromString(strObjectId))
          val arguments = Seq(newValue)

          val functionDeclaration =
            """function (target,varName,varValue) {
              |  target[varName] = varValue;
              |}
            """.stripMargin

          val argString = ScriptEvaluateSupport.serializeArgumentValues(arguments, useNamedObject).head
          val expression = s"($functionDeclaration).call(null,$scopeName,'$varName',$argString)"

          // TODO: Stack frame ID should be something else here, to avoid the use of magic strings
          evaluate(scriptHost, "$top", expression, namedObjects, true)

          () // don't return anything

        case Left(problem) =>
          throw new IllegalArgumentException(problem)
      }

  }

  override protected def handleScriptEvent: PartialFunction[ScriptEvent, Unit] = {
    case hb: HitBreakpoint => pauseBasedOnBreakpoint(hb)

    case ScriptAdded(script) => self ! EmitScriptParsed(script)

    case Resumed =>
      emitEvent("Debugger.resumed", null)
  }

  private def scopeType(s: HostScope): String = s.scopeType match {
    case ScopeType.Local => "local"
    case ScopeType.Global => "global"
    case ScopeType.Closure => "closure"
    case ScopeType.With => "with"
  }

  private def pauseBasedOnBreakpoint(hitBreakpoint: HitBreakpoint): Unit = {
    val converter = RemoteObjectConverter.byReference
    def toRemoteObject(value: ValueNode) = converter.toRemoteObject(value)
    val callFrames = hitBreakpoint.stackFrames.map { sf =>
      val scopes = sf.scopeChain.map(s => Scope(scopeType(s), toRemoteObject(s.value)))
      val thisObj = toRemoteObject(sf.thisObj)
      // Reuse stack frame ID as call frame ID so that mapping is easier when we talk to the debugger
      CallFrame(sf.id, Location(sf.breakpoint.scriptId, sf.breakpoint.lineNumberBase1 - 1, 0), scopes, thisObj, sf.functionDetails.name)
    }

    hitBreakpoint.stackFrames.headOption match {
      case Some(sf) =>
        // Call frames are saved to be used with setVariableValue.
        lastCallFrameList = Some(callFrames)
        
        val params = PausedEventParams(callFrames, "other", Seq(sf.breakpoint.breakpointId))
        emitEvent("Debugger.paused", params)

      case None =>
        log.warn("Unexpected! Got a HitBreakpoint without stack frames!")
    }
  }

}
