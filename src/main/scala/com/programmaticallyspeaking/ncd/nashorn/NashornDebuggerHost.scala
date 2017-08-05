package com.programmaticallyspeaking.ncd.nashorn

import java.util.Collections

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, Undefined}
import com.programmaticallyspeaking.ncd.infra.{DelayedFuture, IdGenerator}
import com.programmaticallyspeaking.ncd.messaging.{Observable, Observer, Subject, Subscription}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi.event._
import com.sun.jdi.request.{BreakpointRequest, EventRequest}
import com.sun.jdi.{StackFrame => _, _}
import org.slf4s.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}


object NashornDebuggerHost {
  import scala.collection.JavaConverters._

  val InitialScriptResolveAttempts = 5

  val NIR_ScriptRuntime = "jdk.nashorn.internal.runtime.ScriptRuntime"
  val NIR_ECMAException = "jdk.nashorn.internal.runtime.ECMAException"
  val NIR_Context = "jdk.nashorn.internal.runtime.Context"
  val JL_Boolean = "java.lang.Boolean"
  val JL_Integer = "java.lang.Integer"
  val JL_Long = "java.lang.Long"
  val JL_Double = "java.lang.Double"

  object JDWP_ERR_INVALID_SLOT {
    def unapply(v: Any): Option[Throwable] = v match {
      case e: InternalException if e.errorCode() == 35 => Some(e)
      case _ => None
    }
  }

  // The name of the DEBUGGER method in the ScriptRuntime class
  val ScriptRuntime_DEBUGGER = "DEBUGGER"

  val ECMAException_create = "create"

  // Note that NIR_ECMAException isn't here because requiring it for init will prevent init from completing
  // since we won't see the type until it's used. Also, we never use it in foundWantedTypes lookup.
  val wantedTypes = Set(
    NIR_ScriptRuntime,
    NIR_Context,
    JL_Boolean,
    JL_Integer,
    JL_Long,
    JL_Double
  )

  type CodeEvaluator = (String, Map[String, AnyRef]) => ValueNode

  private def scriptSourceField(refType: ReferenceType): Field = {
    // Generated script classes has a field named 'source'
    Option(refType.fieldByName("source"))
      .getOrElse(throw new Exception("Found no 'source' field in " + refType.name()))
  }

  /**
    * This method extracts the source code of an evaluated script, i.e. a script that hasn't been loaded from a file
    * and therefore doesn't have a path/URL. The official way to do this would be to call `DebuggerSupport.getSourceInfo`,
    * but we cannot do that because when we connect to the VM and discover all scripts, we don't have a thread that is
    * in the appropriate state to allow execution of methods. However, extracting field values is fine, so we dive deep
    * down in the Nashorn internals to grab the raw source code data.
    *
    * @param refType the type of the generated script class
    * @return a source code string
    */
  private[NashornDebuggerHost] def shamelesslyExtractEvalSourceFromPrivatePlaces(refType: ReferenceType): Option[String] = {
    val sourceField = scriptSourceField(refType)
    // Get the Source instance in that field
    Option(refType.getValue(sourceField).asInstanceOf[ObjectReference]).map { source =>
      // From the instance, get the 'data' field, which is of type Source.Data
      val dataField = Option(source.referenceType().fieldByName("data"))
        .getOrElse(throw new Exception("Found no 'data' field in " + source.referenceType().name()))
      // Get the Source.Data instance, which should be a RawData instance
      val data = source.getValue(dataField).asInstanceOf[ObjectReference]
      // Source.RawData has a field 'array' of type char[]
      val charArrayField = Option(data.referenceType().fieldByName("array"))
        .getOrElse(throw new Exception("Found no 'array' field in " + data.referenceType().name()))
      // Get the char[] data
      val charData = data.getValue(charArrayField).asInstanceOf[ArrayReference]
      // Get individual char values from the array
      val chars = charData.getValues.asScala.map(v => v.asInstanceOf[CharValue].charValue())
      // Finally combine into a string
      chars.mkString
    }
  }

  /**
    * An operation that [[NashornDebuggerHost]] send to itself after a small delay in order to retry script resolution
    * for a given referent type.
    *
    * @param referenceType a reference type that is a script but doesn't have an attached source yet.
    */
  case class ConsiderReferenceType(referenceType: ReferenceType, howManyTimes: Int) extends NashornScriptOperation

  case object PostponeInitialize extends NashornScriptOperation

  private[nashorn] class PausedData(val thread: ThreadReference, val stackFrames: Seq[StackFrame],
                                                val marshaller: Marshaller, val isAtDebuggerStatement: Boolean) {

    /** We assume that we can cache object properties as long as we're in a paused state. Since we're connected to a
      * Java process, an arbitrary Java object may change while in this state, so we only cache JS objects.
      */
    val objectPropertiesCache = mutable.Map[ObjectPropertiesKey, Map[String, ObjectPropertyDescriptor]]()

    val propertyHolderCache = mutable.Map[ObjectId, Option[PropertyHolder]]()
  }

  private[nashorn] case class ObjectPropertiesKey(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean)

  /** This marker is embedded in all scripts evaluated by NashornDebuggerHost on behalf of Chrome DevTools. The problem
    * this solves is that such evaluated scripts are detected on startup (i.e. when reconnecting to a running target)
    * but they are not interesting to show in DevTools. Thus NashornDebuggerHost will not consider scripts that contain
    * this marker.
    */
  val EvaluatedCodeMarker = "__af4caa215e04411083cfde689d88b8e6__"

  // Prefix for synthetic properties added to artificial scope JS objects. The prefix is chosen so that it can never
  // clash with the name of a real local variable.
  val hiddenPrefix = "||"

  val hiddenPrefixEscapedForUseInJavaScriptRegExp = "[|][|]"

  val localScopeObjectIdPrefix = "$$locals-"
  val stackFrameIndexExtraProp = hiddenPrefix + "stackIndex"

  case class ObjectDescriptor(native: Option[Value], marshalled: ComplexNode, extras: Map[String, ValueNode])

  private val infrastructureThreadNames = Set("attach listener", "reference handler", "finalizer", "signal dispatcher")

  def isInfrastructureThread(t: ThreadReference) = infrastructureThreadNames.contains(t.name().toLowerCase)
  def isRunningThread(t: ThreadReference) = t.status() == ThreadReference.THREAD_STATUS_RUNNING

  val IL_POP = 0x57     // pop result after function return
  val IL_ARETURN = 0xb0 // return reference from function
  val IlCodesToIgnoreOnStepEvent = Set(IL_POP, IL_ARETURN)

  /**
    * Information used to determine if a breakpoint request event is in the exact same location as the previous step
    * event. When we do expensive step into after stepping using a StepRequest, it seems as if the BreakpointRequest
    * is hit in the current location right away.
    */
  case class StepLocationInfo(location: Location, stackSize: Int)
  object StepLocationInfo {
    def from(ev: LocatableEvent) = StepLocationInfo(ev.location(), ev.thread().frameCount())
  }

  // Filter for step requests for stopping in a script
  val StepRequestClassFilter = "jdk.nashorn.internal.scripts.*"

  /**
    * Key for an EventRequest property that stores a handler to execute when an event for the request is seen.
    */
  private[nashorn] object EventHandlerKey

  /**
    * The type of a handler for an event associated with an event request.
    */
  type EventHandler = (Event) => Unit

  case class StackFrameImpl(id: String, thisObj: ValueNode, scopeChain: Seq[Scope],
                            breakableLocation: BreakableLocation,
                            eval: CodeEvaluator,
                            functionDetails: FunctionDetails) extends StackFrame {
    val scriptId = breakableLocation.script.id
    val scriptURL = breakableLocation.script.url
    val location = breakableLocation.scriptLocation
    val nativeLocation = breakableLocation.location
  }

  case class StackFrameHolder(stackFrame: Option[StackFrame], location: Location) {
    val mayBeAtSpecialStatement = stackFrame.isEmpty
    val isAtDebuggerStatement = isDebuggerStatementLocation(location)
  }

  // Determines if the location is in ScriptRuntime.DEBUGGER.
  private def isDebuggerStatementLocation(loc: Location) =
    loc.declaringType().name() == NIR_ScriptRuntime && loc.method().name() == ScriptRuntime_DEBUGGER

}

class NashornDebuggerHost(val virtualMachine: VirtualMachine, protected val asyncInvokeOnThis: ((NashornScriptHost) => Any) => Future[Any])
    extends NashornScriptHost with Logging with ProfilingSupport with ObjectPropertiesSupport with StepSupport with BreakpointSupport with PauseSupport {
  import NashornDebuggerHost._
  import com.programmaticallyspeaking.ncd.infra.BetterOption._

  import ExecutionContext.Implicits._
  import scala.collection.JavaConverters._

  private val scriptByPath = mutable.Map[String, Script]()

  protected val breakableLocationsByScriptUrl = mutable.Map[String, ListBuffer[BreakableLocation]]()

  protected val enabledBreakpoints = mutable.Map[String, ActiveBreakpoint]()

  private val scriptIdGenerator = new IdGenerator("nds")
  protected val breakpointIdGenerator = new IdGenerator("ndb")
  protected val stackframeIdGenerator = new IdGenerator("ndsf")

  private val eventSubject = Subject.serialized[ScriptEvent]

  private var hostInitializationComplete = false

  protected val objectDescriptorById = mutable.Map[ObjectId, ObjectDescriptor]()

  private val objectReferencesWithDisabledGC = ListBuffer[ObjectReference]()

  private val objectReferencesWithDisabledGCForTheEntireSession = ListBuffer[ObjectReference]()

  private var infoAboutLastStep: Option[StepLocationInfo] = None

  protected var currentExceptionPauseType: ExceptionPauseType = ExceptionPauseType.None

  private val mappingRegistry: MappingRegistry = (value: Value, valueNode: ComplexNode, extra: Map[String, ValueNode]) => {
    objectDescriptorById += valueNode.objectId -> ObjectDescriptor(Option(value), valueNode, extra)
  }

  protected val foundWantedTypes = mutable.Map[String, ClassType]()

  private val scriptTypesWaitingForSource = ListBuffer[ReferenceType]()
  private val scriptTypesToBreakRetryCycleFor = ListBuffer[ReferenceType]()

  // Data that are defined when the VM has paused on a breakpoint or encountered a step event
  protected var pausedData: Option[PausedData] = None

  /**
    * Configure what we do when we encounter one of the wanted types.
    */
  private val actionPerWantedType: Map[String, () => Unit] = Map(
    NIR_ScriptRuntime -> enableBreakingAtDebuggerStatement _
  )

  /**
    * By default, we don't pause when a breakpoint is hit. This is important since we add a fixed breakpoint for
    * JS 'debugger' statements, and we don't want that to pause the VM when a debugger hasn't attached yet.
    */
  protected var willPauseOnBreakpoints = false

  /**
    * If true, we won't stop on breakpoints or exceptions.
    */
  protected var disablePausingAltogether = false

  private var seenClassPrepareRequests = 0
  private var lastSeenClassPrepareRequests = -1L
  private var hasScannedClasses = false

  private val typeLookup = new TypeLookup {
    def apply(name: String): Option[ClassType] = foundWantedTypes.get(name)
  }
  private val boxer = new Boxer(typeLookup)
  protected val codeEval = new CodeEval(typeLookup)
  private val stackBuilder = new StackBuilder(stackframeIdGenerator, typeLookup, mappingRegistry, codeEval, boxer,
    (location: Location) => findBreakableLocation(location))

  /**
    * Associate a handler to be executed when an event for the request is observed.
    */
  protected def beforeEventIsHandled(r: EventRequest)(h: EventHandler): Unit = {
    Option(r.getProperty(EventHandlerKey)).foreach(_ => throw new IllegalMonitorStateException("Event handler already associated."))
    r.putProperty(EventHandlerKey, h)
  }

  private def addBreakableLocations(script: Script, breakableLocations: Seq[BreakableLocation]): Unit = {
    breakableLocationsByScriptUrl.getOrElseUpdate(script.url.toString, ListBuffer.empty) ++= breakableLocations
  }

  private def enableBreakingAt(typeName: String, methodName: String, statementName: String): Unit = {
    val methodLoc = for {
      theType <- foundWantedTypes.get(typeName).toEither(s"no $typeName type found")
      theMethod <- theType.methodsByName(methodName).asScala.headOption.toEither(s"$typeName.$methodName method not found")
      location <- theMethod.allLineLocations().asScala.headOption.toEither(s"no line location found in $typeName.$methodName")
    } yield location

    methodLoc match {
      case Right(location) =>
        log.info(s"Enabling automatic breaking at JavaScript '$statementName' statements")
        // TODO: BreakableLocation also does this. Reuse code!
        val br = virtualMachine.eventRequestManager().createBreakpointRequest(location)
        br.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
        br.setEnabled(true)
      case Left(msg) =>
        log.warn(s"Won't be able to break at JavaScript '$statementName' statements because $msg")
    }
  }

  private def enableBreakingAtDebuggerStatement(): Unit =
    enableBreakingAt(NIR_ScriptRuntime, ScriptRuntime_DEBUGGER, "debugger")

  private def scriptFromEval(refType: ReferenceType, scriptPath: String, attemptsLeft: Int): Either[String, Script] = {
    shamelesslyExtractEvalSourceFromPrivatePlaces(refType) match {
      case Some(src) =>
        // NOTE: The Left here is untested. Our test setup doesn't allow us to connect multiple times to
        // the same VM, in which case we could observe these "leftover" scripts.
        if (src.contains(EvaluatedCodeMarker)) Left("it contains internally evaluated code")
        else Right(getOrAddEvalScript(scriptPath, src))
      case None =>
        val willRetry = attemptsLeft > 2
        if (willRetry) {

          // Since a breakpoint may be hit before our retry attempt, we add the reference type to our list
          // ouf source-less types. If we hit a breakpoint, we try to "resolve" all references in that list.
          scriptTypesWaitingForSource += refType

          // I couldn't get a modification watchpoint to work (be triggered). Perhaps it's because the 'source'
          // field is set using reflection? So instead retry in a short while.
          val item = ConsiderReferenceType(refType, attemptsLeft - 1)
          DelayedFuture(50.milliseconds) {
            asyncInvokeOnThis(_.handleOperation(item))
          }
        }

        val retryStr = if (willRetry) ", will retry" else ""
        Left(s"no source available (yet$retryStr)")
    }
  }

  private def guessColumns(script: Script): Unit = {
    import com.programmaticallyspeaking.ncd.infra.TraversableExtensions._
    breakableLocationsByScriptUrl.get(script.url.toString) match {
      case Some(locations) =>
        locations.distinctBy(_.location.method()).groupBy(_.scriptLocation.lineNumber1Based).foreach {
          case (lineNo, locs) if locs.size > 1 =>
            val sortedLocs = locs.sortWith((bl1, bl2) => {
              val bl1Method = bl1.location.method()
              val bl2Method = bl2.location.method()

              // # = CompilerConstants.NESTED_FUNCTION_SEPARATOR
              val bl1MethodNameParts = bl1Method.name().split("#")
              val bl2MethodNameParts = bl2Method.name().split("#")
              bl2MethodNameParts.startsWith(bl1MethodNameParts)
            })
            val columns = script.statementColumnsForLine(lineNo)
            sortedLocs.zip(columns).foreach {
              case (bl, col) => bl.setColumn(col)
            }
          case _ => // single loc
        }

      case None => // noop
    }
  }

  private def registerScript(script: Script, scriptPath: String, locations: Seq[Location]): Unit = {
    val isKnownScript = breakableLocationsByScriptUrl.contains(script.url.toString)

    val erm = virtualMachine.eventRequestManager()
    val breakableLocations = locations.map(l => new BreakableLocation(script, erm, l))
    addBreakableLocations(script, breakableLocations)
    try guessColumns(script) catch {
      case NonFatal(t) =>
        log.error(s"Column guessing failed for ${script.url}", t)
    }

    if (isKnownScript) {
      log.debug(s"Reusing script with URI '${script.url}' for script path '$scriptPath'")
    } else {
      // Reason for logging double at different levels: info typically goes to the console, debug to the log file.
      log.debug(s"Adding script at path '$scriptPath' with ID '${script.id}' and URI '${script.url}'")
      log.info(s"Adding script with URI '${script.url}'")
      emitEvent(ScriptAdded(script))
    }
  }

  private def handleScriptResult(result: Try[Either[String, Script]], refType: ReferenceType, scriptPath: String, locations: Seq[Location], attemptsLeft: Int): Option[Script] = result match {
    case Success(Right(script)) =>
      registerScript(script, scriptPath, locations)
      Some(script)
    case Success(Left(msg)) =>
      log.debug(s"Ignoring script because $msg")
      None
    case Failure(t) =>
      log.error(s"Ignoring script at path '$scriptPath'", t)
      None
  }

  private def considerReferenceType(refType: ReferenceType, attemptsLeft: Int): Option[Script] = {
    if (attemptsLeft == 0) return None

    val className = refType.name()

    if (wantedTypes.contains(className)) {
      refType match {
        case ct: ClassType =>
          log.debug(s"Found the $className type")
          foundWantedTypes += className -> ct

          // Execute any function associated with the type
          actionPerWantedType.get(className).foreach(_.apply())

          // If we have all types, we're done
          if (wantedTypes.forall(foundWantedTypes.contains)) {
            considerInitializationToBeComplete()
          }

        case other =>
          log.warn(s"Found the $className type but it's a ${other.getClass.getName} rather than a ClassType")
      }
      None
    } else if (className.startsWith("jdk.nashorn.internal.scripts.Script$")) {
      // This is a compiled Nashorn script class.
      log.debug(s"Script reference type: ${refType.name} ($attemptsLeft attempts left)")

      Try(refType.allLineLocations().asScala) match {
        case Success(locations) =>
          locations.headOption match {
            case Some(firstLocation) =>
              val scriptPath = scriptPathFromLocation(firstLocation)

              val triedScript: Try[Either[String, Script]] = Try {
                // Note that we no longer try to use the script path for reading the source. If the script contains a
                // sourceURL annotation, Nashorn will use that at script path, so we might end up reading CoffeeScript
                // source instead of the real source.
                scriptFromEval(refType, scriptPath, attemptsLeft)
              }

              handleScriptResult(triedScript, refType, scriptPath, locations, attemptsLeft)
            case None =>
              log.info(s"Ignoring script type '${refType.name} because it has no line locations.")
              None
          }
        case Failure(t: AbsentInformationException) =>
          log.warn(s"No line locations for ${refType.name}")
          None
        case Failure(t) =>
          log.warn(s"Failed to get line locations for ${refType.name}", t)
          None
      }
    } else None
  }

  private def watchAddedClasses(): Unit = {
    val request = virtualMachine.eventRequestManager().createClassPrepareRequest()
    request.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    request.setEnabled(true)
  }

  private def retryInitLater(): Unit = {
    log.debug("Postponing initialization until classes have stabilized")
    DelayedFuture(200.milliseconds) {
      asyncInvokeOnThis(_.handleOperation(PostponeInitialize))
    }
  }

  def initialize(): Unit = {
    watchAddedClasses()
    retryInitLater()
  }

  //TODO: This method is never called, because there's no graceful way to stop NCDbg ATM.
  def prepareForExit(): Unit = {
    try {
      objectReferencesWithDisabledGCForTheEntireSession.foreach(_.enableCollection())
    } catch {
      case NonFatal(t) =>
        log.error("Failed to enable collection for one or more object references.", t)
    }
  }

  private def scanClasses(): Unit = {
    log.debug("Scanning all currently known classes...")
    val referenceTypes = virtualMachine.allClasses()

    // Go through reference types that exist so far. More may arrive later!
    referenceTypes.asScala.foreach(considerReferenceType(_: ReferenceType, InitialScriptResolveAttempts))

    hasScannedClasses = true
  }

  private def considerInitializationToBeComplete(): Unit = {
    log.info("Host initialization is complete.")
    hostInitializationComplete = true
    emitEvent(InitialInitializationComplete)
  }

  private def emitEvent(event: ScriptEvent): Unit = {
    // Emit asynchronously so that code that observes the event can interact with the host without deadlocking it.
    log.debug(s"Emitting event of type ${event.getClass.getSimpleName}")
    Future(eventSubject.onNext(event))
  }

  private def signalComplete(): Unit = {
    // Emit asynchronously so that code that observes the event can interact with the host without deadlocking it.
    log.info("Signalling completion.")
    Future(eventSubject.onComplete())
  }

  // toList => iterate over a copy since we mutate inside the foreach
  private def attemptToResolveSourceLessReferenceTypes(): Unit = scriptTypesWaitingForSource.toList match {
    case Nil => // noop
    case xs =>
      log.info(s"Attempting to source-resolve ${scriptTypesWaitingForSource.size} script type(s)")
      xs.foreach { refType =>
        // Only 1 attempt because we don't want retry on this, since we don't want multiple retry "loops" going on in
        // parallel.
        considerReferenceType(refType, 1) match {
          case Some(_) =>
            scriptTypesWaitingForSource -= refType
            scriptTypesToBreakRetryCycleFor += refType
          case None => // no luck
        }
      }
  }

  private def hasDeathOrDisconnectEvent(eventSet: EventSet) = eventSet.asScala.collect {
    case e: VMDeathEvent => e
    case e: VMDisconnectEvent => e
  }.nonEmpty

  private def removeAnyStepRequest(): Unit = {
    val erm = virtualMachine.eventRequestManager()
    erm.deleteEventRequests(erm.stepRequests())
  }

  private def byteCodeFromLocation(location: Location): Int =  {
    val methodByteCodes = location.method().bytecodes()
    val bc = methodByteCodes(location.codeIndex().toInt).toInt
    if (bc < 0) bc + 256 else bc
  }

  private def handleStepOrMethodEntryEvent(ev: LocatableEvent): Boolean = {
    var doResume = true
    attemptToResolveSourceLessReferenceTypes()
    virtualMachine.eventRequestManager().deleteEventRequest(ev.request())
    val bc = byteCodeFromLocation(ev.location())
    if (IlCodesToIgnoreOnStepEvent.contains(bc)) {
      // We most likely hit an "intermediate" location after returning from a function.
      log.debug(s"Skipping step/method entry event at ${ev.location()} because byte code is ignored: 0x${bc.toHexString}")
      createEnabledStepOverRequest(ev.thread(), isAtDebuggerStatement = false)
    } else {
      log.debug(s"Considering step/method entry event at ${ev.location()} with byte code: 0x${bc.toHexString}")
      // forcePause = true, because: Stepping should work even if breakpoints are disabled, and method entry is
      // when the user wants to pause, which also should work when breakpoints are disabled.
      doResume = handleBreakpoint(ev, pauseEvenIfBreakpointsAreDisabled = true)
      if (!doResume) infoAboutLastStep = Some(StepLocationInfo.from(ev))
    }
    doResume
  }

  private def catchLocationIsNative(frames: Seq[StackFrameHolder], catchLocation: Location): Boolean = {
    val catchMethod = catchLocation.method()
    val isScriptCatchLocation = frames.exists(f => f.location.method() == catchMethod && f.stackFrame.isDefined)
    !isScriptCatchLocation
  }

  private def throwLocationIsScript(frames: Seq[StackFrameHolder]): Boolean = frames.headOption.flatMap(_.stackFrame).isDefined

  def handleOperation(eventQueueItem: NashornScriptOperation): Unit = eventQueueItem match {
    case NashornEventSet(es) if hasDeathOrDisconnectEvent(es) =>
      signalComplete()
    case NashornEventSet(eventSet) =>
      var doResume = true
      eventSet.asScala.foreach { ev =>
        try {
          // Invoke any event handler associated with the request for the event.
          Option(ev.request().getProperty(EventHandlerKey)).foreach {
            case h: EventHandler => h(ev)
          }

          ev match {
            case ev: MethodEntryEvent if pausedData.isEmpty =>
              doResume = handleStepOrMethodEntryEvent(ev)

            case ev: StepEvent if pausedData.isEmpty =>
              doResume = handleStepOrMethodEntryEvent(ev)

            case ev: BreakpointEvent if pausedData.isEmpty =>
              infoAboutLastStep match {
                case Some(info) if info == StepLocationInfo.from(ev) =>
                  // We stopped in the same location. Continue!
                  log.debug(s"Breakpoint event in the same location (${ev.location()}) as the previous step event. Ignoring!")
                case _ =>
                  removeAnyStepRequest()
                  attemptToResolveSourceLessReferenceTypes()

                  doResume = handleBreakpoint(ev)
              }

            case ev: ClassPrepareEvent =>
              if (hasScannedClasses) {
                // A new class, added after we have scanned
                considerReferenceType(ev.referenceType(), InitialScriptResolveAttempts)
              } else {
                // Bump the class counter - when we handle PostponeInitialize, if the class cound has stabilized,
                // we do a full scan.
                log.debug(s"New class (${ev.referenceType().name()}), counting it to await full scan when class count has stabilized.")
                seenClassPrepareRequests += 1
              }
            case ev: ExceptionEvent if pausedData.isEmpty =>
              attemptToResolveSourceLessReferenceTypes()

              val exceptionTypeName = ev.exception().referenceType().name()
              val isECMAException = exceptionTypeName == NIR_ECMAException
              if (isECMAException) {
                // TODO: Move this code into PauseSupport...
                val stackPredicate = { frames: Seq[StackFrameHolder] =>
                  val throwingInScript = throwLocationIsScript(frames)
                  if (throwingInScript) {
                    // If there is no catch location, we know this is an uncaught exception.
                    // If there is a catch location but no script frames beyond the current frame, then assume the catch
                    // location is in native code and treat the exception as effectively uncaught.
                    val isUncaught = ev.catchLocation() == null || catchLocationIsNative(frames, ev.catchLocation())
                    val caughtStr = if (isUncaught) "uncaught" else "caught"
                    val pauseOnCaught = currentExceptionPauseType == ExceptionPauseType.Caught || currentExceptionPauseType == ExceptionPauseType.All
                    val pauseOnUncaught = currentExceptionPauseType == ExceptionPauseType.Uncaught || currentExceptionPauseType == ExceptionPauseType.All
                    val catchIt = (isUncaught && pauseOnUncaught) || (!isUncaught && pauseOnCaught)
                    log.debug(s"Exception $exceptionTypeName is $caughtStr; pausing on caught: $pauseOnCaught, uncaught: $pauseOnUncaught")
                    if (catchIt) None
                    else Some(s"the exception is $caughtStr and we're not pausing on that kind")
                  } else Some("the exception is thrown in a non-script frame")
                }
                doResume = handleBreakpoint(ev, pauseEvenIfBreakpointsAreDisabled = true, stackPredicate)
              }

            case _: VMStartEvent =>
              // ignore it, but don't log a warning

            case other if pausedData.isDefined =>
              // Don't react on events if we're paused. Only one thread can be debugged at a time. Only log this on
              // trace level to avoid excessive logging in a multi-threaded system.
              val eventName = other.getClass.getSimpleName
              log.trace(s"Ignoring Nashorn event $eventName since we're already paused.")

            case other =>
              log.warn("Unknown event: " + other)
          }
          // Clear info about the last step as soon as we see a non-step event.
          if (!ev.isInstanceOf[StepEvent]) {
            infoAboutLastStep = None
          }
        } catch {
          case ex: Exception =>
            log.error(s"Failed to handle event ${ev.getClass.getName}", ex)
        }
      }
      if (doResume) resume(eventSet)
    case ConsiderReferenceType(refType, attemptsLeft) =>
      // We may have resolved the reference type when hitting a breakpoint, and in that case we can ignore this retry
      // attempt.
      if (scriptTypesToBreakRetryCycleFor.contains(refType)) {
        scriptTypesToBreakRetryCycleFor -= refType
      } else {
        considerReferenceType(refType, attemptsLeft)
      }
    case PostponeInitialize =>
      if (lastSeenClassPrepareRequests == seenClassPrepareRequests) scanClasses()
      else {
        lastSeenClassPrepareRequests = seenClassPrepareRequests
        retryInitLater()
      }
    case operation =>
      throw new IllegalArgumentException("Unknown operation: " + operation)
  }

  private def resume(eventSet: EventSet): Unit = {
    enableGarbageCollectionWhereDisabled()
    eventSet.resume()
  }

  protected def functionDetails(functionMethod: Method): FunctionDetails = {
    FunctionDetails(functionMethod.name())
  }

  private def enableGarbageCollectionWhereDisabled(): Unit = {
    objectReferencesWithDisabledGC.foreach(_.enableCollection())
    objectReferencesWithDisabledGC.clear()
  }

  protected def disableGarbageCollectionFor(value: Value, entireSession: Boolean = false): Unit = value match {
    case objRef: ObjectReference =>
      // Disable and track the reference so we can enable when we resume
      objRef.disableCollection()
      val targetList = if (entireSession) objectReferencesWithDisabledGCForTheEntireSession else objectReferencesWithDisabledGC
      targetList += objRef
    case _ =>
  }

  private def createMarshaller()(implicit threadReference: ThreadReference): Marshaller = {
    new Marshaller(mappingRegistry) {
      override def marshal(value: Value): ValueNode = {
        disableGarbageCollectionFor(value)
        super.marshal(value)
      }
    }
  }

  private def handleBreakpoint(ev: LocatableEvent, pauseEvenIfBreakpointsAreDisabled: Boolean = false, stackPredicate: Seq[StackFrameHolder] => Option[String] = _ => None): Boolean = {
    def ignoreIt(reason: String) = {
      log.debug(s"Ignoring breakpoint at ${ev.location()} because $reason.")
      true
    }

    // disablePausingAltogether disabled all sort of pausing - exceptions, stepping, breakpoints...
    if (disablePausingAltogether) return ignoreIt("pausing is entirely disabled")

    // Resume right away if we're not pausing on breakpoints
    if (!willPauseOnBreakpoints && !pauseEvenIfBreakpointsAreDisabled) return ignoreIt("breakpoints are disabled")

    // Log at debug level because we get noise due to exception requests.
    log.debug(s"A breakpoint was hit at location ${ev.location()} in thread ${ev.thread().name()}")
    implicit val thread = ev.thread()

    // Start with a fresh object registry
    objectDescriptorById.clear()

    // Shared marshaller
    implicit val marshaller = createMarshaller()

    val stackFrames = stackBuilder.captureStackFrames(thread)
    stackPredicate(stackFrames) match {
      case Some(ignoreReason) => return ignoreIt(ignoreReason)
      case None =>
    }

    stackFrames.headOption match {
      case Some(holder) if holder.stackFrame.isEmpty && !holder.mayBeAtSpecialStatement =>
        // First/top stack frame doesn't belong to a script. Resume!
        ignoreIt("it doesn't belong to a script")
      case Some(holder) =>
        if (holder.isAtDebuggerStatement) log.debug("Breakpoint is at JavaScript 'debugger' statement")
        val didPause = doPause(thread, stackFrames.flatMap(_.stackFrame), holder.isAtDebuggerStatement)
        // Resume will be controlled externally
        !didPause // false
      case None =>
        // Hm, no stack frames at all... Resume!
        ignoreIt("no stack frames were found at all")
    }
  }

  private def doPause(thread: ThreadReference, stackFrames: Seq[StackFrame], atDebugger: Boolean)(implicit marshaller: Marshaller): Boolean = {
    stackFrames.headOption.collect { case sf: StackFrameImpl => sf } match {
      case Some(topStackFrame) =>
        val scriptId = topStackFrame.scriptId
        val activeBreakpoint = getActiveBreakpoint(topStackFrame.breakableLocation)
        val breakpointId = activeBreakpoint.id

        // Check condition if we have one. We cannot do this until now (which means that a conditional breakpoint
        // will be slow) because we need stack frames and locals to be setup for code evaluation.
        val conditionIsTrue = activeBreakpoint.condition match {
          case Some(c) =>
            topStackFrame.eval(c, Map.empty) match {
              case SimpleValue(true) => true
              case SimpleValue(false) =>
                log.trace(s"Not pausing on breakpoint $breakpointId in script $scriptId since the condition ($c) evaluated to false.")
                false
              case other =>
                log.warn(s"Condition $c resulted in unexpected value $other, will pause.")
                // It's best to pause since we don't know what happened, I think.
                true
            }
          case None => true // no condition, always stop
        }

        if (conditionIsTrue) {
          // Indicate that we're paused
          pausedData = Some(new PausedData(thread, stackFrames, marshaller, atDebugger))

          findScript(ScriptIdentity.fromId(scriptId)).foreach { s =>
            val location = topStackFrame.location
            val line = s.sourceLine(location.lineNumber1Based).getOrElse("<unknown line>")
            log.info(s"Pausing at ${s.url}:${location.lineNumber1Based}: $line")
          }

          val hitBreakpoint = HitBreakpoint(stackFrames, breakpointId)
          emitEvent(hitBreakpoint)
        }
        conditionIsTrue
      case None =>
        log.debug("Not pausing because there are no stack frames")
        false
    }
  }

  private def scriptPathFromLocation(location: Location): String = {
    // It appears *name* is a path on the form 'file:/c:/...', whereas path has a namespace prefix
    // (jdk\nashorn\internal\scripts\). This seems to be consistent with the documentation (although it's a bit
    // surprising), where it is stated that the Java stratum doesn't use source paths and a path therefore is a
    // package-qualified file name in path form, whereas name is the unqualified file name (e.g.:
    // java\lang\Thread.java vs Thread.java).
    val path = location.sourceName()
    if (path == "<eval>") {
      // For evaluated scripts, convert the type name into something that resembles a file URI.
      val typeName = location.declaringType().name()
      "eval:/" + typeName
        .replace("jdk.nashorn.internal.scripts.", "")
        .replace('.', '/')
        .replace('\\', '/')
        .replaceAll("[$^_]", "")
        .replaceFirst("/eval/?$", "")
    } else {
      path // keep it simple
    }
  }

  private def getOrAddEvalScript(artificialPath: String, source: String): Script = {
    val isRecompilation = artificialPath.contains("Recompilation")
    val newScript = ScriptImpl.fromSource(artificialPath, source, scriptIdGenerator.next)
    if (!isRecompilation) return scriptByPath.getOrElseUpdate(artificialPath, newScript)

    // For a recompilation, we will (most likely) already have the original script that was recompiled (recompilation
    // happens for example when a function inside the eval script is called with known types). We find the original
    // script by comparing contents hashes. If we find the original script, we just discard the new one and use the
    // original.
    scriptByPath.values.find(_.contentsHash() == newScript.contentsHash()) match {
      case Some(scriptWithSameSource) =>
        // Note that we add a map entry for the original script with the new path as key. This way we'll find our
        // reused script using all its "alias paths".
        // Note 2: I worry that comparing contents hashes isn't enough - that we need to verify no overlapping
        // line locations also. But we don't have locations here, and I don't want to do too much defensive coding.
        scriptByPath += (artificialPath -> scriptWithSameSource)
        scriptWithSameSource
      case None =>
        scriptByPath.getOrElseUpdate(artificialPath, newScript)
    }
  }

  override def scripts: Seq[Script] = scriptByPath.values.toSeq

  override def findScript(id: ScriptIdentity): Option[Script] = {
    val predicate = id match {
      case IdBasedScriptIdentity(x) => (s: Script) => s.id == x
      case URLBasedScriptIdentity(url) => (s: Script) => s.url.toString == url
    }
    scripts.find(predicate) //TODO: make more efficient
  }


  override def events: Observable[ScriptEvent] = new Observable[ScriptEvent] {
    override def subscribe(observer: Observer[ScriptEvent]): Subscription = {
      // Make sure the observer sees that we're initialized
      if (hostInitializationComplete) {
        observer.onNext(InitialInitializationComplete)
      }
      eventSubject.subscribe(observer)
    }
  }

  protected def findBreakableLocation(location: Location): Option[BreakableLocation] = {
    scriptByPath.get(scriptPathFromLocation(location)).flatMap { script =>
      // We cannot compare locations directly because the passed-in Location may have a code index that is
      // different from the one stored in a BreakableLocation - so do a line comparison.
      val id = ScriptIdentity.fromURL(script.url)
      findBreakableLocationsAtLine(id, location.lineNumber()).flatMap(_.find(bl => sameMethodAndLine(bl.location, location)))
    }
  }

  private def sameMethodAndLine(l1: Location, l2: Location): Boolean = {
    l1.method() == l2.method() && l1.lineNumber() == l2.lineNumber()
  }

  protected def findActiveBreakpoint(location: Location): Option[ActiveBreakpoint] = {
    findBreakableLocation(location).map(getActiveBreakpoint)
  }

  private def getActiveBreakpoint(bl: BreakableLocation): ActiveBreakpoint = {
    enabledBreakpoints.values.find(_.contains(bl)) match {
      case Some(ab) => ab // existing active breakpoint
      case None => ActiveBreakpoint(breakpointIdGenerator.next, Seq(bl), None) // temporary breakpoint (e.g. debugger statement)
    }
  }

  protected def findBreakableLocationsAtLine(id: ScriptIdentity, lineNumber: Int): Option[Seq[BreakableLocation]] = {
    id match {
      case _: IdBasedScriptIdentity =>
        findScript(id) match {
          case Some(script) =>
            findBreakableLocationsAtLine(ScriptIdentity.fromURL(script.url), lineNumber)
          case None => throw new IllegalArgumentException("Unknown script: " + id)
        }
      case URLBasedScriptIdentity(scriptUrl) =>
        breakableLocationsByScriptUrl.get(scriptUrl).map { breakableLocations =>
          breakableLocations.filter(_.scriptLocation.lineNumber1Based == lineNumber)
        }
    }

  }

  protected def resumeWhenPaused(): Unit = pausedData match {
    case Some(data) =>
      log.info("Resuming virtual machine")
      enableGarbageCollectionWhereDisabled()
      virtualMachine.resume()
      pausedData = None
      objectDescriptorById.clear() // only valid when paused
      emitEvent(Resumed)
    case None =>
      log.debug("Ignoring resume request when not paused (no pause data).")
  }

  private def removeAllBreakpoints(): Unit = {
    enabledBreakpoints.foreach(e => e._2.disable())
    enabledBreakpoints.clear()
  }

  override def reset(): Unit = {
    log.info("Resetting VM...")
    willPauseOnBreakpoints = false
    removeAllBreakpoints()
    resume()
  }

  override def removeBreakpointById(id: String): Unit = {
    enabledBreakpoints.get(id) match {
      case Some(activeBp) =>
        log.info(s"Removing breakpoint with id $id")
        activeBp.disable()
        enabledBreakpoints -= activeBp.id
      case None =>
        log.warn(s"Got request to remove an unknown breakpoint with id $id")
    }
  }

  private def evaluateOnStackFrame(pd: PausedData, stackFrameId: String, expression: String, namedObjects: Map[String, ObjectId]): ValueNode = {
    findStackFrame(pd, stackFrameId) match {
      case Some(sf: StackFrameImpl) =>
        implicit val marshaller = pd.marshaller

        // Get the Value instances corresponding to the named objects
        val namedValues = namedObjects.flatMap {
          case (name, objectId) =>
            objectDescriptorById.get(objectId) match {
              // TODO: Should we handle extras here?
              case Some(descriptor) if descriptor.native.isDefined => Seq(name -> descriptor.native.get)
              case Some(_) => Seq.empty
              case _ =>
                throw new IllegalArgumentException(s"No object with ID '$objectId' was found.")
            }
        }

        // Evaluating code may modify any existing object, which means that we cannot keep our object properties
        // cache. There's no point trying to be smart here and only remove entries for the named objects, since the
        // code may call a function that modifies an object that we don't know about here.
        pd.objectPropertiesCache.clear()

        // By resetting change tracking before evaluating the expression, we can track changes made to any
        // named objects.
        resetChangeTracking(sf, namedValues)

        val result = sf.eval(expression, namedValues)

        // Update locals that changed, if needed. It's not sufficient for the synthetic locals object to have
        // been updated, since generated Java code will access the local variables directly.
        updateChangedLocals(sf, namedValues, namedObjects)

        result
      case _ =>
        log.warn(s"No stack frame found with ID $stackFrameId. Available IDs: " + pd.stackFrames.map(_.id).mkString(", "))
        throw new IllegalArgumentException(s"Failed to find a stack frame with ID $stackFrameId")
    }
  }

  override def evaluateOnStackFrame(stackFrameId: String, expression: String, namedObjects: Map[String, ObjectId]): Try[ValueNode] = Try {
    pausedData match {
      case Some(pd) => evaluateOnStackFrame(pd, stackFrameId, expression, namedObjects)
      case None =>
        log.warn(s"Evaluation of '$expression' for stack frame $stackFrameId cannot be done in a non-paused state.")
        throw new IllegalStateException("Code evaluation can only be done in a paused state.")
    }
  }

  private def resetChangeTracking(sf: StackFrameImpl, namedValues: Map[String, AnyRef]): Unit = {
    val objectNames = namedValues.keys.mkString(",")
    val js =
      s"""[$objectNames].forEach(function (obj) {
         |  if(typeof obj['${hiddenPrefix}resetChanges']==='function') obj['${hiddenPrefix}resetChanges']();
         |});
       """.stripMargin
    sf.eval(js, namedValues) match {
      case ErrorValue(data, _, _) =>
        throw new RuntimeException("Failed to reset change tracking: " + data.message)
      case _ =>
    }
  }

  // If the type name is something like 'int', i.e. without a dot, it's bound to be primitive. I think.
  private def typeNameLooksPrimitive(typeName: String) = typeName.indexOf('.') < 0

  private def updateChangedLocals(sf: StackFrameImpl, namedValues: Map[String, AnyRef], namedObjects: Map[String, ObjectId])(implicit marshaller: Marshaller): Unit = {
    def jdiStackFrameForObject(id: ObjectId) =
      objectDescriptorById.get(id).flatMap(_.extras.get(stackFrameIndexExtraProp)).flatMap(_.as[Number]).map(n => marshaller.thread.frame(n.intValue()))

    // Note: namedValues is created from namedObjects, so we access namedObjects directly (not via get)
    namedValues.map(e => (e._1, e._2, namedObjects(e._1))).foreach {
      case (key, value, objectId) =>
        // Read the changes tracked by the property setters, if any.
        val changes = sf.eval(s"$key['${hiddenPrefix}changes']", Map(key -> value))
        arrayValuesFrom(changes) match {
          case Right(values) if values.nonEmpty =>

            // Get the stack frame. We cannot do that earlier due to marshalling, which causes the thread to resume.
            jdiStackFrameForObject(objectId) match {
              case Some(jdiStackFrame) =>

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

              case None =>
                log.warn(s"Failed to find the stack frame hosting $objectId")
            }
          case Right(_) => // empty changes, noop
          case Left(reason) =>
            log.warn(s"Failed to read changes from $key: $reason")
        }
    }
  }

  private def arrayValuesFrom(vn: ValueNode)(implicit marshaller: Marshaller): Either[String, List[Value]] = {
    vn match {
      case an: ArrayNode =>
        objectDescriptorById.get(an.objectId).flatMap(_.native) match {
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

  private def findStackFrame(pausedData: PausedData, id: String): Option[StackFrame] = {
    if (id == "$top") return pausedData.stackFrames.headOption
    pausedData.stackFrames.find(_.id == id)
  }

  override def restartStackFrame(stackFrameId: String): Seq[StackFrame] = {
    pausedData match {
      case Some(pd) =>
        log.warn(s"Request to restart stack frame $stackFrameId. Note that depending on the Java version of the target, this may cause the target to crash.")
        // Get the Location of the stack frame to pop
        pd.stackFrames.find(_.id == stackFrameId) match {
          case Some(sf: StackFrameImpl) =>
            val location = sf.nativeLocation

            // Now get the current stack frame list and identify the correct target. This is needed since the old
            // stack frame list isn't valid anymore (due to thread resume due to marshalling).
            pd.thread.frames().asScala.find(_.location() == location) match {
              case Some(jdiStackFrame) =>
                log.debug(s"Popping stack frame at location ${jdiStackFrame.location()}")

                // ThreadReference.popFrames(StackFrame) API doc:
                // "All frames up to and including the frame are popped off the stack. The frame previous to the
                // parameter frame will become the current frame. After this operation, this thread will be suspended
                // at the invoke instruction of the target method that created frame. The frame's method can be
                // reentered with a step into the instruction.
                pd.thread.popFrames(jdiStackFrame)

                // Don't grab new frames - simply assume that we can reuse a slice of the current list.
                pd.stackFrames.span(_ ne sf)._2.tail

              case None =>
                throw new IllegalArgumentException("Unknown stack frame location: " + location)
            }
          case _ =>
            val availableIdsAsString = pd.stackFrames.map(_.id).mkString(", ")
            throw new IllegalArgumentException(s"Stack frame ID '$stackFrameId' is unknown. Available: $availableIdsAsString")
        }

      case None =>
        throw new IllegalStateException("Frame restart can only be done in a paused state.")
    }
  }
}

object InitialInitializationComplete extends ScriptEvent

/**
  * An active breakpoint may map to one or more breakable locations, since we cannot distinguish between location
  * column numbers.
  *
  * @param id the breakpoint ID
  * @param breakableLocations the breakable locations
  */
case class ActiveBreakpoint(id: String, breakableLocations: Seq[BreakableLocation], condition: Option[String]) {
  assert(breakableLocations.nonEmpty, "An active breakpoint needs at least one breakable location")

  val firstBreakableLocation = breakableLocations.head

  def toBreakpoint: Breakpoint = {
    val script = firstBreakableLocation.script
    Breakpoint(id, script.id, Some(script.url), breakableLocations.map(_.scriptLocation))
  }

  def disable(): Unit = breakableLocations.foreach(_.disable())
  def enable(): Unit = breakableLocations.foreach(_.enable())

  def contains(breakableLocation: BreakableLocation) = breakableLocations.contains(breakableLocation)
}

