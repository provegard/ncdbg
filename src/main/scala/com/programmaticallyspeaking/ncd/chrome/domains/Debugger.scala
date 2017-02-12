package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.host.{Scope => HostScope, _}
import org.slf4s.Logging

object Debugger {
  type CallFrameId = String

  case object stepOver
  case object stepInto
  case object stepOut

  case object resume

  case class evaluateOnCallFrame(callFrameId: CallFrameId, expression: String, silent: Option[Boolean], returnByValue: Option[Boolean], generatePreview: Option[Boolean])

  case class EvaluateOnCallFrameResult(result: Runtime.RemoteObject, exceptionDetails: Option[Runtime.ExceptionDetails] = None)

  case class ScriptParsedEventParams(scriptId: String, url: String, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int, executionContextId: Int, hash: String)

  case class setBreakpointsActive(active: Boolean)

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
      script.contentsHash())
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
}

class Debugger extends DomainActor with Logging with ScriptEvaluateSupport with RemoteObjectConversionSupport {
  import Debugger._

  override def postStop(): Unit = try {
    // Tell the ScriptHost to reset, so that we don't leave it paused
    Option(scriptHost).foreach(_.reset())
  } finally super.postStop()

  override def handle = {
    case Domain.enable =>
      log.info("Enabling debugging, sending all parsed scripts to the client.")
      scriptHost.scripts.foreach { script =>
        emitEvent("Debugger.scriptParsed", ScriptParsedEventParams(script))
      }

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
      log.info(s"Setting breakpoint in '$url' at line $lineNumberBase1.")
      val bp = scriptHost.setBreakpoint(url, lineNumberBase1)

      SetBreakpointByUrlResult(bp.breakpointId, Seq(Location(bp.scriptId, bp.lineNumberBase1 - 1, 0)))

    case Debugger.resume =>
      scriptHost.resume()

    case Debugger.removeBreakpoint(id) =>
      scriptHost.removeBreakpointById(id)

    case Debugger.stepInto =>
      scriptHost.step(StepInto)

    case Debugger.stepOver =>
      scriptHost.step(StepOver)

    case Debugger.stepOut =>
      scriptHost.step(StepOut)

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
  }

  override protected def handleScriptEvent: PartialFunction[ScriptEvent, Unit] = {
    case hb: HitBreakpoint => pauseBasedOnBreakpoint(hb)

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
    def callFrames = hitBreakpoint.stackFrames.map { sf =>
      val scopes = sf.scopeChain.map(s => Scope(scopeType(s), toRemoteObject(s.value)))
      val thisObj = toRemoteObject(sf.thisObj)
      // Reuse stack frame ID as call frame ID so that mapping is easier when we talk to the debugger
      CallFrame(sf.id, Location(sf.breakpoint.scriptId, sf.breakpoint.lineNumberBase1 - 1, 0), scopes, thisObj, sf.functionDetails.name)
    }

    hitBreakpoint.stackFrames.headOption match {
      case Some(sf) =>
        val params = PausedEventParams(callFrames, "other", Seq(sf.breakpoint.breakpointId))
        emitEvent("Debugger.paused", params)

      case None =>
        log.warn("Unexpected! Got a HitBreakpoint without stack frames!")
    }
  }
}
