package com.programmaticallyspeaking.ncd.chrome.domains

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject
import com.programmaticallyspeaking.ncd.chrome.net.FilePublisher
import com.programmaticallyspeaking.ncd.host.{Scope => HostScope, _}
import com.programmaticallyspeaking.ncd.infra.{FileReader, FileSystemFileReader, ScriptURL, SourceMap}
import org.slf4s.Logging

import scala.collection.mutable
import scala.util.{Failure, Success}

object Debugger extends Logging {
  type CallFrameId = String

  case object stepOver
  case object stepInto
  case object stepOut

  case object resume

  case class evaluateOnCallFrame(callFrameId: CallFrameId, expression: String, silent: Option[Boolean], returnByValue: Option[Boolean], generatePreview: Option[Boolean])

  case class EvaluateOnCallFrameResult(result: Runtime.RemoteObject, exceptionDetails: Option[Runtime.ExceptionDetails] = None)

  case class ScriptParsedEventParams(scriptId: String, url: String, startLine: Int, startColumn: Int, endLine: Int, endColumn: Int, executionContextId: Int, hash: String,
                                     hasSourceURL: Boolean, sourceMapURL: Option[String])

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

  /**
    * Continues execution until specific location is reached.
    */
  case class continueToLocation(location: Location)

  object ScriptParsedEventParams {
    def apply(script: Script): ScriptParsedEventParams = new ScriptParsedEventParams(script.id,
      script.url.toString,
      0, // offset
      0, // offset
      Math.max(script.lineCount - 1, 0), // offset
      endColumn = script.lastLineLength, // NOT an offset: "Length of the last line of the script" according to the docs
      Runtime.StaticExecutionContextId,
      script.contentsHash(),
      script.sourceUrl().isDefined,
      script.sourceMapUrl().map(_.toString))
  }

  case class getScriptSource(scriptId: String)

  case class GetScriptSourceResult(scriptSource: String)

  case class GetPossibleBreakpointsResult(locations: Seq[BreakLocation])

  case class setBreakpointByUrl(lineNumber: Int, url: String, columnNumber: Option[Int], condition: Option[String])

  case class removeBreakpoint(breakpointId: String)

  case class restartFrame(callFrameId: CallFrameId)

  case class RestartFrameResult(callFrames: Seq[CallFrame])

  case class SetBreakpointByUrlResult(breakpointId: String, locations: Seq[Location])

  case class Location(scriptId: String, lineNumber: Int, columnNumber: Option[Int])

  /**
    * @param scriptId Script identifier as reported in the Debugger.scriptParsed.
    * @param lineNumber Line number in the script (0-based).
    * @param columnNumber Column number in the script (0-based).
    * @param `type` Allowed values: debuggerStatement, call, return.
    */
  case class BreakLocation(scriptId: String, lineNumber: Int, columnNumber: Option[Int], `type`: Option[String] = None)

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

  def scriptWithPublishedFiles(script: Script, filePublisher: FilePublisher)(implicit fileReader: FileReader): Script = {
    script.sourceMapUrl() match {
      case Some(url) if url.isFile =>
        val sourceMapFile = url.toFile

        // Publish the map file
        val externalUrl = filePublisher.publish(sourceMapFile)

        // Read the map file to find the source file(s)
        fileReader.read(sourceMapFile, StandardCharsets.UTF_8).map(SourceMap.fromJson) match {
          case Success(sourceMap) =>

            // Publish all sources, but we're not interested in their URLs.
            // TODO: Should they be relative to the map rather?
            sourceMap.sources.map(script.url.resolve).filter(_.isFile).foreach(u => filePublisher.publish(u.toFile))

            new ProxyScript(script) {
              override def sourceMapUrl(): Option[ScriptURL] = Some(ScriptURL.create(externalUrl))
            }

          case Failure(t) =>
            log.error(s"Failed to read source map file $sourceMapFile", t)
            script
        }
      case _ => script
    }
  }

  private def uriToFile(uri: String): File = {
    // Assume uri begins with file:// (TODO: Create a value type for it!)
    val fileURI = new URI(uri.replace("file://", "file:/"))
    new File(fileURI)
  }
}

class Debugger(filePublisher: FilePublisher, scriptHost: ScriptHost) extends DomainActor(scriptHost) with Logging with ScriptEvaluateSupport with RemoteObjectConversionSupport {
  import Debugger._
  import com.programmaticallyspeaking.ncd.infra.BetterOption._

  private implicit val fileReader = new FileSystemFileReader
  private implicit val host = scriptHost

  override def postStop(): Unit = try {
    // Tell the ScriptHost to reset, so that we don't leave it paused
    Option(scriptHost).foreach(_.reset())
  } finally super.postStop()

  /** Key = script ID
    * Value = contents hash
    */
  private val emittedScripts = mutable.Map[String, String]()

  private var lastCallFrameList: Option[Seq[CallFrame]] = None

  private var temporaryBreakpointIds = Set[String]()

  private def emitScriptParsedEvent(script: Script) = {
    val hash = script.contentsHash()
    if (emittedScripts.getOrElse(script.id, "") == hash) {
      log.trace(s"Won't re-emit scriptParsed event for script with ID '${script.id}' and same hash ($hash) as before.")
    } else {
      emittedScripts += script.id -> hash
      val modifiedScript = scriptWithPublishedFiles(script, filePublisher)
      emitEvent("Debugger.scriptParsed", ScriptParsedEventParams(modifiedScript))
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

    case Domain.disable =>
      // Prepare for re-enabling
      emittedScripts.clear()

    case Debugger.getScriptSource(scriptId) =>
      log.debug(s"Requested script source for script with ID $scriptId")
      scriptHost.findScript(ScriptIdentity.fromId(scriptId)) match {
        case Some(script) =>
          GetScriptSourceResult(script.contents)
        case None =>
          throw new IllegalArgumentException("Unknown script ID: " + scriptId)
      }

    case Debugger.setBreakpointByUrl(lineNumberBase0, url, maybeColumnNumberBase0, condition) =>
      // DevTools passes "" when the breakpoint isn't conditional
      val actualCondition = condition.filter(_ != "")
      val location = ScriptLocation(lineNumberBase0 + 1, maybeColumnNumberBase0.map(_ + 1))
      scriptHost.setBreakpoint(ScriptIdentity.fromURL(url), location, actualCondition) match {
        case Some(bp) =>
          SetBreakpointByUrlResult(bp.breakpointId, bp.locations.map(l => Location(bp.scriptId, l.lineNumber1Based - 1, l.columnNumber1Based.map(_ - 1))))
        case None =>
          val loc = lineNumberBase0 + maybeColumnNumberBase0.map(c => ":" + c).getOrElse("")
          log.warn(s"Cannot identify breakpoint at $url:$loc")
          //TODO: Huh, should this be an error instead??
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
      val locations = scriptHost.getBreakpointLocations(ScriptIdentity.fromId(start.scriptId), ScriptLocation(start.lineNumber + 1, start.columnNumber.map(_ + 1)),
        maybeEnd.map(e => ScriptLocation(e.lineNumber + 1, e.columnNumber.map(_ + 1))))
        .map(loc => BreakLocation(start.scriptId, loc.lineNumber1Based - 1, loc.columnNumber1Based.map(_ - 1)))
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
          implicit val remoteObjectConverter = createRemoteObjectConverter(false, false)

          val namedObjects = new NamedObjects

          val scopeName = namedObjects.useNamedObject(ObjectId.fromString(strObjectId))
          val newValueName = ScriptEvaluateSupport.serializeArgumentValues(Seq(newValue), namedObjects).head

          val expression = s"$scopeName['$varName']=$newValueName;"

          // TODO: Stack frame ID should be something else here, to avoid the use of magic strings
          evaluate(scriptHost, "$top", expression, namedObjects.result, true).exceptionDetails match {
            case Some(details) =>
              val location = details.url.map(u => " (at " + Seq(u, details.lineNumber.toString, details.columnNumber.toString).mkString(":") + ")").getOrElse("")
              throw new IllegalArgumentException(details.text + location)
            case None =>
              () // don't return anything
          }

        case Left(problem) =>
          throw new IllegalArgumentException(problem)
      }

    case Debugger.restartFrame(callFrameId) =>
      stackFramesToCallFrames(scriptHost.restartStackFrame(callFrameId))

    case Debugger.continueToLocation(location) =>
      // DevTools always use column 0 (there's a comment: "Always use 0 column."), but if we pass a column to
      // the host, it will be too picky, so pass no column at all.
      val scriptLocation = ScriptLocation(location.lineNumber + 1, None)
      scriptHost.setBreakpoint(ScriptIdentity.fromId(location.scriptId), scriptLocation, None) match {
        case Some(bp) =>
          log.debug(s"Continue to location with temporary breakpoint ID ${bp.breakpointId}")
          temporaryBreakpointIds += bp.breakpointId

          scriptHost.resume()

        case None =>
          throw new IllegalArgumentException(s"Failed to continue to location $location, couldn't set a breakpoint there.")
      }
  }

  override protected def handleScriptEvent: PartialFunction[ScriptEvent, Unit] = {
    case hb: HitBreakpoint =>
      pauseBasedOnBreakpoint(hb).foreach(breakpointId => {
        if (temporaryBreakpointIds.contains(breakpointId)) {
          // This was a temporary breakpoint, so remove it
          temporaryBreakpointIds -= breakpointId
          scriptHost.removeBreakpointById(breakpointId)
        }
      })

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

  private def stackFramesToCallFrames(frames: Seq[StackFrame]): Seq[CallFrame] = {
    val converter = RemoteObjectConverter.byReference
    def toRemoteObject(value: ValueNode) = converter.toRemoteObject(value)
    frames.map { sf =>
      val scopes = sf.scopeChain.map(s => Scope(scopeType(s), toRemoteObject(s.value)))
      val thisObj = toRemoteObject(sf.thisObj)
      // Reuse stack frame ID as call frame ID so that mapping is easier when we talk to the debugger
      val headLocation = sf.breakpoint.locations.head
      CallFrame(sf.id, Location(sf.breakpoint.scriptId, headLocation.lineNumber1Based - 1, headLocation.columnNumber1Based.map(_ - 1)), scopes, thisObj, sf.functionDetails.name)
    }
  }

  private def pauseBasedOnBreakpoint(hitBreakpoint: HitBreakpoint): Option[String] = {
    val callFrames = stackFramesToCallFrames(hitBreakpoint.stackFrames)
    hitBreakpoint.stackFrames.headOption match {
      case Some(sf) =>
        // Call frames are saved to be used with setVariableValue.
        lastCallFrameList = Some(callFrames)
        
        val params = PausedEventParams(callFrames, "other", Seq(sf.breakpoint.breakpointId))
        emitEvent("Debugger.paused", params)
        Some(sf.breakpoint.breakpointId)

      case None =>
        log.warn("Unexpected! Got a HitBreakpoint without stack frames!")
        None
    }
  }

}
