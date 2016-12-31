package com.programmaticallyspeaking.ncd.chrome.domains

import java.io.File

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.host._
import org.slf4s.Logging

import scala.util.{Failure, Success}

object Debugger {
  type CallFrameId = String

  case object stepOver
  case object stepInto
  case object stepOut

  case object resume

  case class evaluateOnCallFrame(callFrameId: CallFrameId, expression: String, silent: Option[Boolean])

  case class EvaluateOnCallFrameResult(result: Runtime.RemoteObject, exceptionDetails: Option[Runtime.ExceptionDetails] = None)

  case class ScriptParsedEventParams(scriptId: String, url: String, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int, executionContextId: Int, hash: String)

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

class Debugger extends DomainActor with Logging {
  import Debugger._

  private var remoteObjectConverter: RemoteObjectConverter = _


  override def postStop(): Unit = try {
    // Tell the ScriptHost to reset, so that we don't leave it paused
    Option(scriptHost).foreach(_.reset())
  } finally super.postStop()

  override protected def scriptHostReceived(): Unit = {
    remoteObjectConverter = new RemoteObjectConverter()
  }

  override def handle = {
    case Domain.enable =>
      log.info("Enabling debugging, sending all parsed scripts to the client.")
      scriptHost.scripts.foreach { script =>
        emitEvent("Debugger.scriptParsed", ScriptParsedEventParams(script))
      }

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

    case Debugger.evaluateOnCallFrame(callFrameId, expression, maybeSilent) =>
      // TODO: "In silent mode exceptions thrown during evaluation are not reported and do not pause execution. Overrides setPauseOnException state."
      // TODO: -- Obey setPauseOnException - and what about pausing??

      // TODO: What is the exception ID for?
      val exceptionId = 1

      val reportException = !maybeSilent.getOrElse(false)
      scriptHost.evaluateOnStackFrame(callFrameId, expression, Map.empty) match {
        case Success(err: ErrorValue) if reportException =>
          val data = err.data
          // Note that Chrome wants line numbers to be 0-based
          val details = Runtime.ExceptionDetails(exceptionId, data.message, data.lineNumberBase1 - 1, data.columnNumber, Some(data.url))
          // Apparently we need to pass an actual value with the exception details
          EvaluateOnCallFrameResult(RemoteObject.undefinedValue, Some(details))
        case Success(err: ErrorValue) =>
          EvaluateOnCallFrameResult(RemoteObject.undefinedValue)
        case Success(result) => EvaluateOnCallFrameResult(toRemoteObject(result))
        case Failure(t) =>

          val exceptionDetails = t.getStackTrace.headOption.flatMap { stackTraceElement =>
            try {
              val lineNumberBase1 = stackTraceElement.getLineNumber
              val url = new File(stackTraceElement.getFileName).toURI.toString
              Some(Runtime.ExceptionDetails(exceptionId, t.getMessage, lineNumberBase1 - 1, 0, Some(url)))
            } catch {
              case e: Exception =>
                log.error(s"Error when trying to construct ExceptionDetails from $stackTraceElement", e)
                None
            }
          }.getOrElse(Runtime.ExceptionDetails(exceptionId, t.getMessage, 0, 0, None))

          EvaluateOnCallFrameResult(RemoteObject.undefinedValue, Some(exceptionDetails))
      }
  }

  override protected def handleScriptEvent: PartialFunction[ScriptEvent, Unit] = {
    case hb: HitBreakpoint => pauseBasedOnBreakpoint(hb)

    case Resumed =>
      emitEvent("Debugger.resumed", null)
  }

  private def toRemoteObject(node: ValueNode): RemoteObject = remoteObjectConverter.toRemoteObject(node)

  private def pauseBasedOnBreakpoint(hitBreakpoint: HitBreakpoint): Unit = {
    def callFrames = hitBreakpoint.stackFrames.map { sf =>
      val localScope = Scope("local", toRemoteObject(sf.locals))
      val scopes = Seq(localScope) ++ sf.scopeObj.map(s => Scope("closure", toRemoteObject(s))).toSeq
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
