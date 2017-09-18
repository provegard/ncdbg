package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, Undefined}
import com.programmaticallyspeaking.ncd.infra.{DelayedFuture, IdGenerator, PathUtils}
import com.programmaticallyspeaking.ncd.messaging.{Observable, Observer, Subject, Subscription}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{ObjectPropertiesKey, StackFrameHolder}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi.event._
import com.sun.jdi.request.EventRequest
import com.sun.jdi.{StackFrame => _, _}
import org.slf4s.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}


object NashornDebuggerHost {
  import TypeConstants._
  import JDIExtensions._

  val InitialScriptResolveAttempts = 5

  object JDWP_ERR_INVALID_SLOT {
    def unapply(v: Any): Option[Throwable] = v match {
      case e: InternalException if e.errorCode() == 35 => Some(e)
      case _ => None
    }
  }

  val ECMAException_create = "create"

  val wantedTypes = Map(
    NIR_ScriptRuntime -> true,
    NIR_Context -> true,
    JL_Boolean -> true,
    JL_Integer -> true,
    JL_Long -> true,
    JL_Double -> true,
    NIO_Global -> true,
    NIR_ECMAException -> false, // don't stop init
    NIR_JSType -> false // don't stop init
  )

  type CodeEvaluator = (String, Map[String, AnyRef]) => ValueNode

  /**
    * An operation that [[NashornDebuggerHost]] send to itself after a small delay in order to retry script resolution
    * for a given referent type.
    *
    * @param referenceType a reference type that is a script but doesn't have an attached source yet.
    */
  case class ConsiderReferenceType(referenceType: ReferenceType, howManyTimes: Int) extends NashornScriptOperation

  case object PostponeInitialize extends NashornScriptOperation

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

  /**
    * Scanning all classes in one go can be problematic since it may take a long time, thereby blocking host
    * operations. This constant defines the time we spend scanning classes before we yield to other operations.
    */
  val SCAN_CLASSES_BATCH_LEN = 2.seconds

  /**
    * Operation queued when the scan-class batch length has been exceeded.
    */
  case object ContinueClassScan extends NashornScriptOperation

  /**
    * Information used to determine if a breakpoint request event is in the exact same location as the previous step
    * event. When we do expensive step into after stepping using a StepRequest, it seems as if the BreakpointRequest
    * is hit in the current location right away.
    */
  case class StepLocationInfo(location: Location, stackSize: Int)
  object StepLocationInfo {
    def from(ev: LocatableEvent) = StepLocationInfo(ev.location(), ev.thread().frameCount())
  }

  val ScriptClassNamePrefix = "jdk.nashorn.internal.scripts.Script$"

  /**
    * Key for an EventRequest property that stores a handler to execute when an event for the request is seen.
    */
  private[nashorn] object EventHandlerKey

  /**
    * The type of a handler for an event associated with an event request.
    */
  type EventHandler = (Event, Option[PausedData]) => Boolean

  case class StackFrameImpl(id: String, thisObj: ValueNode, scopeChain: Seq[Scope],
                            breakableLocation: BreakableLocation,
                            eval: CodeEvaluator,
                            functionDetails: FunctionDetails) extends StackFrame {
    val scriptId = breakableLocation.script.id
    val scriptURL = breakableLocation.script.url
    val location = breakableLocation.scriptLocation
    val nativeLocation = breakableLocation.location
  }

  object ExceptionType {
    sealed trait EnumVal
    case object CaughtByScript extends EnumVal { override def toString = "caught" }
    case object UncaughtByScript extends EnumVal { override def toString = "uncaught" }
    case object Unknown extends EnumVal { override def toString = "unknown" }

    def determine(catchLocation: Location, stackFrames: Seq[StackFrameHolder]): EnumVal = {
      catchLocation match {
        case loc if loc == null => UncaughtByScript
        case loc =>
          val catchMethod = loc.method()
          def isNotCatchFrame(sf: StackFrameHolder) = sf.location.method() != catchMethod
          val framesFromCatchLocation = stackFrames.span(isNotCatchFrame)._2.toList
          framesFromCatchLocation match {
            case x :: _ if x.belongsToScript => CaughtByScript
            case _ :: rest =>
              // Catch location is a non-script. If there are no script frames beyond the catch location, then the
              // exception is uncaught. Otherwise, it's impossible to know (since we cannot get the exception table
              // via JDI).
              val hasScriptFrameAfterCatchLocation = rest.exists(_.belongsToScript)
              if (hasScriptFrameAfterCatchLocation) Unknown else UncaughtByScript
            case Nil =>
              // Couldn't find catch frame...
              Unknown
          }
      }
    }
  }

  object NoScriptReason {
    sealed trait EnumVal
    case object NoSource extends EnumVal
    case object EvaluatedCode extends EnumVal
  }

  case class StackFrameHolder(stackFrame: Option[StackFrame], location: Location) {
    val mayBeAtSpecialStatement = stackFrame.isEmpty
    val belongsToScript = stackFrame.isDefined
    val isAtDebuggerStatement = location.isDebuggerStatement
  }
}

class NashornDebuggerHost(val virtualMachine: VirtualMachine, protected val asyncInvokeOnThis: ((NashornScriptHost) => Any) => Future[Any])
    extends NashornScriptHost with Logging with ProfilingSupport with ObjectPropertiesSupport with StepSupport with BreakpointSupport with PauseSupport with PrintSupport {
  import NashornDebuggerHost._
  import JDIExtensions._
  import TypeConstants._
  import Breakpoints._
  import ExecutionContext.Implicits._
  import scala.collection.JavaConverters._

  private val scriptByPath = mutable.Map[String, Script]()

  protected val breakableLocationsByScriptUrl = mutable.Map[String, Seq[BreakableLocation]]()

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

  /**
    * Populated/updated during a class scan to track the classes left to scan.
    */
  private var classesToScan = List.empty[ReferenceType]

  private val scriptTypesWaitingForSource = ListBuffer[ReferenceType]()
  private val scriptTypesToBreakRetryCycleFor = ListBuffer[ReferenceType]()

  // Data that are defined when the VM has paused on a breakpoint or encountered a step event
  protected var pausedData: Option[PausedData] = None

  /**
    * Configure what we do when we encounter one of the wanted types.
    */
  private val actionPerWantedType: Map[String, (ClassType) => Unit] = Map(
    NIR_ScriptRuntime -> enableBreakingAtDebuggerStatement _,
    NIR_ECMAException -> enableExceptionPausing _,
    NIO_Global -> enablePrintCapture _
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
  private var hasInitiatedClassScanning = false

  private val typeLookup = new TypeLookup {
    def apply(name: String): Option[ClassType] = foundWantedTypes.get(name)
  }
  private val boxer = new Boxer(typeLookup)
  protected val codeEval = new CodeEval(typeLookup, preventGC)
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
    val existing = breakableLocationsByScriptUrl.getOrElse(script.url.toString, Seq.empty)

    // Remove existing ones where Location is unset and the line number exists among the new ones. These are placeholders
    // added so that it's possible to set a breakpoint in a function before it has been executed, but now we have real
    // locations for that function (supposedly).
    // TODO: This may be broken if multiple one-liner functions are defined on the same line...
    val lineNumbersOfNewOnes = breakableLocations.map(_.scriptLocation.lineNumber1Based).toSet

    // TODO: Identify BLs no longer relevant due to recompilation. Perhaps by function node ID? If such a BL
    // TODO: is enabled, it needs to be disabled.
    def isObsolete(bl: BreakableLocation) = bl.location.isEmpty && lineNumbersOfNewOnes.contains(bl.scriptLocation.lineNumber1Based)

    val lineNumbersOfEnabled = existing.filter(_.isEnabled).map(_.scriptLocation.lineNumber1Based).toSet

    // Auto-enable a BL if we have an existing one that is enabled for the same line number - regardless of whether
    // it's a placeholder or not. This is untested for non-placeholders, since we probably need a big test script
    // to trigger this case.
    def shouldEnable(bl: BreakableLocation) = lineNumbersOfEnabled.contains(bl.scriptLocation.lineNumber1Based)

    breakableLocations.filter(shouldEnable).foreach { bl =>
      log.debug(s"Auto-enabling breakable location $bl since it's on the same line as a currently enabled one.")
      bl.enable()
    }

    val newList = existing.filterNot(isObsolete) ++ breakableLocations
    breakableLocationsByScriptUrl(script.url.toString) = newList
  }

  private def scriptFromEval(refType: ReferenceType, scriptPath: String): Either[NoScriptReason.EnumVal, Script] = {
    refType.shamelesslyExtractEvalSourceFromPrivatePlaces().map { src =>
      if (src.contains(EvaluatedCodeMarker)) Left(NoScriptReason.EvaluatedCode)
      else Right(getOrAddEvalScript(scriptPath, src))
    }.getOrElse(Left(NoScriptReason.NoSource))
  }

  private def potentiallyBreakableLines(script: Script): Seq[Int] = {
    def hasRelevantContent(line: String) = {
      val trimmed = line.trim()
      trimmed != "" && trimmed != "{" && trimmed != "}"
    }
    def looksBreakable(line: Int) = script.sourceLine(line).exists(hasRelevantContent)
    (1 to script.lineCount).filter(looksBreakable)
  }

  private def gatherBreakableLocations(script: Script, locations: Seq[Location]): Seq[BreakableLocation] = {
    val erm = virtualMachine.eventRequestManager()
    // Find all potentially breakable lines. Create breakable locations from actual locations. Then create
    // candidates for the potentially breakable lines that weren't covered. Such a line may for example belong to
    // a function that hasn't been executed yet.
    var lineNumbers = potentiallyBreakableLines(script).toSet
    val breakableLocations = locations.map(l => new BreakableLocation(script, erm, l))
    breakableLocations.foreach(bl => lineNumbers -= bl.scriptLocation.lineNumber1Based)
    breakableLocations ++ lineNumbers.map(line => new BreakableLocation(script, erm, line))
  }

  private def registerScript(script: Script, scriptPath: String, locations: Seq[Location]): Unit = {
    val isKnownScript = breakableLocationsByScriptUrl.contains(script.url.toString)

    addBreakableLocations(script, gatherBreakableLocations(script, locations))

    if (isKnownScript) {
      log.debug(s"Reusing script with URI '${script.url}' for script path '$scriptPath'")
    } else {
      // Reason for logging double at different levels: info typically goes to the console, debug to the log file.
      log.debug(s"Adding script at path '$scriptPath' with ID '${script.id}', URI '${script.url}' and hash '${script.contentsHash()}'")
      log.info(s"Adding script with URI '${script.url}'")
      emitEvent(ScriptAdded(script))
    }
  }

  private def handleScriptResult(maybeThread: Option[ThreadReference], result: Try[Either[NoScriptReason.EnumVal, Script]],
                                 refType: ReferenceType, scriptPath: String, locations: Seq[Location], attemptsLeft: Int): Option[Script] = result match {
    case Success(Right(script)) =>
      registerScript(script, scriptPath, locations)
      Some(script)
    case Success(Left(NoScriptReason.EvaluatedCode)) =>
      log.debug(s"Ignoring script because it contains evaluated code")
      None
    case Success(Left(NoScriptReason.NoSource)) if attemptsLeft > 1 => // hm, was previously 2. Why??
      val contextCodeInstallerType =
        maybeThread.flatMap(t => t.frames.asScala.view.map(f => f.location().method().declaringType()).find(_.name().contains("Context$ContextCodeInstaller")))
      contextCodeInstallerType match {
        case Some(ciType) =>
          log.debug(s"Will get source from ${refType.name()} via ContextCodeInstaller")
          // Let the current method exit in order to hold of the source. This logic is based on the observation that
          // we see the ClassPrepareEvent event inside Context$ContextCodeInstaller, on this line:
          //     Field sourceField = clazz.getDeclaredField(CompilerConstants.SOURCE.symbolName());
          // The actual setting of the source happens a few steps later.
          val req = virtualMachine.eventRequestManager().createMethodExitRequest()
          req.addClassFilter(ciType)
          req.setEnabled(true)
          beforeEventIsHandled(req) { (_, _) =>
            log.debug(s"Getting source from ${refType.name()} at method exit from ContextCodeInstaller")
            virtualMachine.eventRequestManager().deleteEventRequest(req)
            considerReferenceType(None, refType, InitialScriptResolveAttempts)
            true // consume the event
          }

          None
        case None =>
          log.debug(s"Will get source from ${refType.name()} by trying again shortly.")
          // Since a breakpoint may be hit before our retry attempt, we add the reference type to our list
          // ouf source-less types. If we hit a breakpoint, we try to "resolve" all references in that list.
          scriptTypesWaitingForSource += refType

          // Consider the reference type in a short while
          val item = ConsiderReferenceType(refType, attemptsLeft - 1)
          DelayedFuture(50.milliseconds) {
            asyncInvokeOnThis(_.handleOperation(item))
          }
          None
      }
    case Success(Left(NoScriptReason.NoSource)) =>
      log.warn(s"Giving up on getting source from ${refType.name()}.")
      None
    case Failure(t) =>
      log.error(s"Ignoring script at path '$scriptPath'", t)
      None
  }

  private def considerReferenceType(thread: Option[ThreadReference], refType: ReferenceType, attemptsLeft: Int): Option[Script] = {
    if (attemptsLeft == 0) return None

    val className = refType.name()

    if (wantedTypes.contains(className)) {
      refType match {
        case ct: ClassType =>
          log.debug(s"Found the $className type")
          foundWantedTypes += className -> ct

          // Execute any function associated with the type
          actionPerWantedType.get(className).foreach(_.apply(ct))

          // If we have all mandatory types, we're done
          val mandatoryTypes = wantedTypes.filter(_._2).keys
          if (mandatoryTypes.forall(foundWantedTypes.contains)) {
            considerInitializationToBeComplete()
          }

        case other =>
          log.warn(s"Found the $className type but it's a ${other.getClass.getName} rather than a ClassType")
      }
      None
    } else if (className.startsWith(ScriptClassNamePrefix)) {
      // This is a compiled Nashorn script class.
      log.debug(s"Script reference type: ${refType.name} ($attemptsLeft attempts left)")

      Try(refType.allLineLocations().asScala) match {
        case Success(locations) =>
          locations.headOption match {
            case Some(firstLocation) =>
              // Note that we no longer try to use the script path for reading the source. If the script contains a
              // sourceURL annotation, Nashorn will use that at script path, so we might end up reading CoffeeScript
              // source instead of the real source.
              val scriptPath = scriptPathFromLocation(firstLocation)
              val triedScript = Try(scriptFromEval(refType, scriptPath))
              handleScriptResult(thread, triedScript, refType, scriptPath, locations, attemptsLeft)

            case None =>
              log.debug(s"Ignoring script type '${refType.name} because it has no line locations.")
              None
          }
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
    val referenceTypes = virtualMachine.allClasses()

    hasInitiatedClassScanning = true

    classesToScan = referenceTypes.asScala.toList

    scanOutstandingClasses()
  }

  private def scanOutstandingClasses(): Unit = {
    log.debug(s"Scanning ${classesToScan.size} currently known classes...")

    val nanosBefore = System.nanoTime()
    var done = false
    var count = 0
    while (!done && classesToScan.nonEmpty) {
      val refType = classesToScan.head
      classesToScan = classesToScan.tail
      count += 1

      considerReferenceType(None, refType, InitialScriptResolveAttempts)

      val elapsed = (System.nanoTime() - nanosBefore).nanos
      if (elapsed >= SCAN_CLASSES_BATCH_LEN) {
        // Yield to other operations, to prevent blocking so long that a timeout occurs in ExecutorProxy.
        log.debug(s"Scanned $count classes in ${elapsed.toMillis} ms, temporarily yielding to other operations.")
        asyncInvokeOnThis(_.handleOperation(ContinueClassScan))
        done = true
      }
    }

    if (classesToScan.isEmpty) {
      log.info("Class scanning complete!")
    }
  }

  private def considerInitializationToBeComplete(): Unit = {
    log.info("Host initialization is complete.")
    hostInitializationComplete = true
    emitEvent(InitialInitializationComplete)
  }

  protected def emitEvent(event: ScriptEvent): Unit = {
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
  private def attemptToResolveSourceLessReferenceTypes(thread: ThreadReference): Unit = scriptTypesWaitingForSource.toList match {
    case Nil => // noop
    case xs =>
      log.info(s"Attempting to source-resolve ${scriptTypesWaitingForSource.size} script type(s)")
      xs.foreach { refType =>
        // Only 1 attempt because we don't want retry on this, since we don't want multiple retry "loops" going on in
        // parallel.
        considerReferenceType(Some(thread), refType, 1) match {
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

  private def shouldIgnoreLocationOnStep(location: Location) = {
    // We should *maybe* ignore IL_POP. Most of the time, it's just popping the return value of a function.
    // However, if that happens on the last line of the method, we'd like to pause inside the callee.
    location.byteCode match {
      case IL_POP => !location.isLastLineInFunction
      case _ => false
    }

  }

  private def handleStepOrMethodEvent(ev: LocatableEvent): Boolean = {
    var doResume = true
    attemptToResolveSourceLessReferenceTypes(ev.thread())
    virtualMachine.eventRequestManager().deleteEventRequest(ev.request())
    if (shouldIgnoreLocationOnStep(ev.location())) {
      // We most likely hit an "intermediate" location after returning from a function.
      log.debug(s"Skipping step/method event at ${ev.location()} with byte code: 0x${ev.location().byteCode.toHexString}")
      createEnabledStepOverRequest(ev.thread(), isAtDebuggerStatement = false)
    } else {
      log.debug(s"Considering step/method event at ${ev.location()} with byte code: 0x${ev.location().byteCode.toHexString}")
      doResume = handleBreakpoint(ev, currentPausedData)
      if (!doResume) infoAboutLastStep = Some(StepLocationInfo.from(ev))
    }
    doResume
  }

  // Creates the PausedData structure
  private def prepareForPausing(ev: LocatableEvent): PausedData = {
    assert(pausedData.isEmpty, "prepareForPausing in paused state")

    // Start with a fresh object registry
    objectDescriptorById.clear()

    // Disable exception requests while we're paused since otherwise an exception thrown during JS evaluation
    // will deadlock (an ExceptionEvent event will be generated but cannot be delivered to NDH since NDH is waiting
    // for the evaluation to complete).
    virtualMachine.eventRequestManager().exceptionRequests().asScala.foreach(_.disable())

    implicit val thread = ev.thread()
    val pd = new PausedData(thread, createMarshaller(), stackBuilder, ev)
    pausedData = Some(pd)
    pd
  }

  private def currentPausedData = pausedData.getOrElse(throw new IllegalStateException("Not paused"))

  private def cleanupPausing(): Unit = {
//    pausedData.foreach()
    pausedData = None
    objectDescriptorById.clear() // only valid when paused

    // Enable exception requests again (see prepareForPausing)
    virtualMachine.eventRequestManager().exceptionRequests().asScala.foreach(_.enable())
  }

  private def shouldPauseOnException(exceptionEventInfo: ExceptionEventInfo): Either[String, Unit] = {
    val pausingAtAll = currentExceptionPauseType != ExceptionPauseType.None
    val pausingOnAllExceptions = currentExceptionPauseType == ExceptionPauseType.All
    def exceptionType = exceptionEventInfo.exceptionType match {
      case ExceptionType.Unknown if !pausingOnAllExceptions =>
        // We have to determine whether to pause or not; the exception may be caught or uncaught but
        // we don't know which.
        // The safest approach seems to be to pause. Chrome/DevTools wants uncaught or all exceptions,
        // never only caught ones (as of this writing), so the worst thing that can happen is that we
        // pause on a caught exception even though we shouldn't.
        log.warn(s"Cannot determine caught-ness of ${exceptionEventInfo.exceptionTypeName} thrown at ${exceptionEventInfo.throwLocation}, will assume uncaught.")
        ExceptionType.UncaughtByScript
      case other => other
    }

    val shouldPause = pausingAtAll && (pausingOnAllExceptions || (exceptionType match {
      case ExceptionType.CaughtByScript => currentExceptionPauseType == ExceptionPauseType.Caught
      case ExceptionType.UncaughtByScript => currentExceptionPauseType == ExceptionPauseType.Uncaught
      case _ => false
    }))

    if (shouldPause) Right(())
    else Left(s"exception is $exceptionType, which is ignored")
  }

  private def shouldPause(pausedData: PausedData, ev: Event): Either[String, Unit] = ev match {
    case _ if disablePausingAltogether =>
      // disablePausingAltogether disabled all sort of pausing - exceptions, stepping, breakpoints...
      Left("pausing is entirely disabled")
    case _ if pausedData.exceptionEventInfo.isDefined =>
      shouldPauseOnException(pausedData.exceptionEventInfo.get)
    case _:StepEvent|_:MethodEntryEvent =>
      // Stepping should work even if breakpoints are disabled, and method entry is when the user wants to pause,
      // which also should work when breakpoints are disabled.
      Right(())
    case _ =>
      // Resume right away if we're not pausing on breakpoints
      if (!willPauseOnBreakpoints) return Left("breakpoints are disabled")
      if (pausedData.stackFrameHolders.isEmpty) return Left("no stack frames were found at all")
      if (!pausedData.pausedInAScript) return Left("location doesn't belong to a script")

      Right(())
  }

  private def maybeEmitErrorEvent(pd: PausedData): Unit = pd.exceptionEventInfo match {
    case Some(info) =>
      if (info.exceptionType == ExceptionType.UncaughtByScript) {
        info.marshalledException match {
          case Right(err) => emitEvent(UncaughtError(err))
          case Left(reason) => log.warn(reason)
        }
      }
    case None =>
  }

  private def relevantForPostponingClassScan(referenceType: ReferenceType) = {
    val name = referenceType.name()
    wantedTypes.contains(name) || name.startsWith(ScriptClassNamePrefix)
  }

  def handleOperation(eventQueueItem: NashornScriptOperation): Unit = eventQueueItem match {
    case NashornEventSet(es) if hasDeathOrDisconnectEvent(es) =>
      signalComplete()
    case NashornEventSet(eventSet) =>
      var doResume = true
      val wasPausedAtEntry = pausedData.isDefined
      eventSet.asScala.foreach { ev =>
        try {
          // Invoke any event handler associated with the request for the event.
          val eventIsConsumed = Option(ev.request().getProperty(EventHandlerKey)).exists {
            case h: EventHandler =>
              val maybePausedData = ev match {
                case lev: LocatableEvent => Some(prepareForPausing(lev))
                case _ => None
              }

              h(ev, maybePausedData)
          }

          if (!eventIsConsumed) {
            // Note: Beyond this point, currentPausedData should work since we prepared for pausing in the
            // before-event handler above.
            ev match {
              case ev: MethodEntryEvent if pausedData.isEmpty =>
                doResume = handleStepOrMethodEvent(ev)

              case ev: MethodExitEvent if pausedData.isEmpty =>
                doResume = handleStepOrMethodEvent(ev)

              case ev: StepEvent if pausedData.isEmpty =>
                doResume = handleStepOrMethodEvent(ev)

              case ev: BreakpointEvent if pausedData.isEmpty =>
                infoAboutLastStep match {
                  case Some(info) if info == StepLocationInfo.from(ev) =>
                    // We stopped in the same location. Continue!
                    log.debug(s"Breakpoint event in the same location (${ev.location()}) as the previous step event. Ignoring!")
                  case _ =>
                    removeAnyStepRequest()
                    attemptToResolveSourceLessReferenceTypes(ev.thread())

                    doResume = handleBreakpoint(ev, currentPausedData)
                }

              case ev: ClassPrepareEvent =>
                if (hasInitiatedClassScanning) {
                  // A new class, added after we have initiated scanning
                  considerReferenceType(Some(ev.thread()), ev.referenceType(), InitialScriptResolveAttempts)
                } else if (relevantForPostponingClassScan(ev.referenceType())) {
                  // Bump the class counter - when we handle PostponeInitialize, if the class cound has stabilized,
                  // we do a full scan.
                  log.trace(s"New class (${ev.referenceType().name()}), counting it to await full scan when class count has stabilized.")
                  seenClassPrepareRequests += 1
                }
              case ev: ExceptionEvent if pausedData.isEmpty =>
                attemptToResolveSourceLessReferenceTypes(ev.thread())

                val exceptionTypeName = ev.exception().referenceType().name()
                val isECMAException = exceptionTypeName == NIR_ECMAException
                if (isECMAException) {
                  val pd = currentPausedData
                  maybeEmitErrorEvent(pd)
                  doResume = handleBreakpoint(ev, pd)
                } else log.trace(s"Ignoring non-ECMA exception of type $exceptionTypeName")

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
          }
        } catch {
          case NonFatal(ex) =>
            log.error(s"Failed to handle event ${ev.getClass.getName}", ex)
        }
      }
      if (doResume) {
        if (!wasPausedAtEntry) cleanupPausing()
        resume(eventSet)
      }
    case ConsiderReferenceType(refType, attemptsLeft) =>
      // We may have resolved the reference type when hitting a breakpoint, and in that case we can ignore this retry
      // attempt.
      if (scriptTypesToBreakRetryCycleFor.contains(refType)) {
        scriptTypesToBreakRetryCycleFor -= refType
      } else {
        considerReferenceType(None, refType, attemptsLeft)
      }
    case PostponeInitialize =>
      if (lastSeenClassPrepareRequests == seenClassPrepareRequests) scanClasses()
      else {
        lastSeenClassPrepareRequests = seenClassPrepareRequests
        retryInitLater()
      }
    case ContinueClassScan =>
      scanOutstandingClasses()
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

  private def preventGC(value: Value, lifecycle: Lifecycle.EnumVal): Unit = {
    disableGarbageCollectionFor(value, lifecycle == Lifecycle.Session)
  }

  private def createMarshaller()(implicit threadReference: ThreadReference): Marshaller = {
    new Marshaller(mappingRegistry) {
      override def marshal(value: Value): ValueNode = {
        disableGarbageCollectionFor(value)
        super.marshal(value)
      }
    }
  }

  private def handleBreakpoint(ev: LocatableEvent, pausedData: PausedData): Boolean = shouldPause(pausedData, ev) match {
    case Left(reason) =>
      log.debug(s"Ignoring breakpoint at ${ev.location()} because $reason.")
      true
    case Right(_) =>
      // Log at debug level because we get noise due to exception requests.
      val details = ev match {
        case ex: ExceptionEvent =>
          s" (due to exception ${ex.exception().referenceType().name()})"
        case _ if pausedData.isAtDebuggerStatement =>
          s" (at a JavaScript 'debugger' statement)"
        case _ => ""
      }
      log.debug(s"Pausing at location ${ev.location()} in thread ${ev.thread().name()}$details")

      // Resume will be controlled externally
      implicit val marshaller = pausedData.marshaller
      !doPause(pausedData.stackFrames)
  }

  private def doPause(stackFrames: Seq[StackFrame])(implicit marshaller: Marshaller): Boolean = {
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
    val newScript = ScriptImpl.fromSource(artificialPath, source, scriptIdGenerator.next)

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
        scriptByPath.get(artificialPath) match {
          case Some(_) =>
            // This is a new script with new contents but with the same path as an old one - it is likely a script
            // that has been reloaded via Nashorn's 'load' function.
            // The choice of using the ID as suffix is pretty arbitrary - it could also be a sequence number.
            val newId = scriptIdGenerator.next
            val newPath = PathUtils.insertNameSuffix(artificialPath, "_" + newId)

            val replacement = ScriptImpl.fromSource(newPath, source, newId)
            scriptByPath.getOrElseUpdate(newPath, replacement)

          case None =>
            scriptByPath.getOrElseUpdate(artificialPath, newScript)
        }
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
      // There may be multiple breakable locations for the same line (even in the same method - e.g. for a 'while'
      // statement that is last in a method). Try to find an exact match first, then fall back to finding a location
      // on the correct line. The fallback is necessary since the passed-in Location may have a code index that is
      // different from the one stored in a BreakableLocation.
      val id = ScriptIdentity.fromURL(script.url)
      findBreakableLocationsAtLine(id, location.lineNumber()).flatMap { bls =>
        bls.find(_.location.contains(location)).orElse(bls.find(bl => sameMethodAndLine(bl.location, location)))
      }
    }
  }

  private def sameMethodAndLine(l1: Option[Location], l2: Location): Boolean = {
    l1.exists(l => l.method() == l2.method() && l.lineNumber() == l2.lineNumber())
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
      cleanupPausing()
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
            pd.thread.frames().asScala.find(f => location.contains(f.location())) match {
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


