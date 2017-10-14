package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.infra.{DelayedFuture, IdGenerator, PathUtils, ScriptURL}
import com.programmaticallyspeaking.ncd.messaging.{Observable, Observer, Subject, Subscription}
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
  import JDIExtensions._
  import TypeConstants._

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
  type EventHandler = (Event) => Boolean

  case class StackFrameImpl(id: String, thisObj: ValueNode, scopeChain: Seq[Scope],
                            breakableLocation: BreakableLocation,
                            eval: CodeEvaluator,
                            functionDetails: FunctionDetails) extends StackFrame {
    val scriptId = breakableLocation.script.id
    val scriptURL = breakableLocation.script.url
    val location = breakableLocation.scriptLocation
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
  import Breakpoints._
  import JDIExtensions._
  import NashornDebuggerHost._
  import TypeConstants._

  import ExecutionContext.Implicits._
  import scala.collection.JavaConverters._

  protected val stackframeIdGenerator = new IdGenerator("ndsf")

  private val eventSubject = Subject.serialized[ScriptEvent]

  private var hostInitializationComplete = false

  protected val objectDescriptorById = mutable.Map[ObjectId, ObjectDescriptor]()

  private val objectReferencesWithDisabledGC = ListBuffer[ObjectReference]()

  private val objectReferencesWithDisabledGCForTheEntireSession = ListBuffer[ObjectReference]()

  private var infoAboutLastStep: Option[StepLocationInfo] = None

  private val mappingRegistry: MappingRegistry = (value: Value, valueNode: ComplexNode, extra: Map[String, ValueNode]) => {
    objectDescriptorById += valueNode.objectId -> ObjectDescriptor(Option(value), valueNode, extra)
  }

  protected val foundWantedTypes = mutable.Map[String, ClassType]()

  /**
    * Populated/updated during a class scan to track the classes left to scan.
    */
  private var classesToScan = List.empty[ReferenceType]

  // Data that are defined when the VM has paused on a breakpoint or encountered a step event
  protected var pausedData: Option[PausedData] = None

  private val _scripts = new Scripts
  protected val _breakableLocations = new BreakableLocations(virtualMachine, _scripts)
  private val _scriptFactory = new ScriptFactory(virtualMachine)
  protected val _breakpoints = new ActiveBreakpoints

  protected val _pauser = new Pauser(_breakpoints, _scripts, emitEvent)

  /**
    * Configure what we do when we encounter one of the wanted types.
    */
  private val actionPerWantedType: Map[String, (ClassType) => Unit] = Map(
    NIR_ScriptRuntime -> enableBreakingAtDebuggerStatement _,
    NIR_ECMAException -> enableExceptionPausing _,
    NIO_Global -> enablePrintCapture _
  )

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

  private var _publishedScriptUrls = Set[ScriptURL]()
  private def publishScript(script: Script): Unit = {
    // Try to ensure that only the first thread observes "isKnownScript" to be true for a particular URL
    val old = _publishedScriptUrls
    _publishedScriptUrls += script.url
    val isKnownScript = old.contains(script.url)

    if (isKnownScript) {
      log.debug(s"Script with URI '${script.url}' is already known")
    } else {
        // Reason for logging double at different levels: info typically goes to the console, debug to the log file.
      log.debug(s"Adding script with ID '${script.id}', URI '${script.url}' and hash '${script.contentsHash()}'")
      log.info(s"Adding script with URI '${script.url}'")
      emitEvent(ScriptAdded(script))
    }
  }

  private def considerReferenceType(thread: Option[ThreadReference], refType: ReferenceType): Unit = {
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
    } else {
      _scriptFactory.considerReferenceType(thread, refType).onComplete {
        case Success(Some(identifiedScript)) =>
          try {
            val script = _scripts.suggest(identifiedScript.script)
            _breakableLocations.add(script, refType.allLineLocations().asScala)
            publishScript(script)
          } catch {
            case NonFatal(t) =>
              log.error("Script publish failure", t)
          }

        case Success(None) => // noop, assume logged elsewhere
        case Failure(t) =>
          log.error("Script reference type error", t)
      }
    }
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

      considerReferenceType(None, refType)

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
    virtualMachine.eventRequestManager().deleteEventRequest(ev.request())
    if (shouldIgnoreLocationOnStep(ev.location())) {
      // We most likely hit an "intermediate" location after returning from a function.
      log.debug(s"Skipping step/method event at ${ev.location()} with byte code: 0x${ev.location().byteCode.toHexString}")
      createEnabledStepOverRequest(ev.thread(), isAtDebuggerStatement = false)
    } else {
      log.debug(s"Considering step/method event at ${ev.location()} with byte code: 0x${ev.location().byteCode.toHexString}")
      doResume = _pauser.handleBreakpoint(ev, prepareForPausing(ev))
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

  private def cleanupPausing(): Unit = {
//    pausedData.foreach()
    pausedData = None
    objectDescriptorById.clear() // only valid when paused

    // Enable exception requests again (see prepareForPausing)
    virtualMachine.eventRequestManager().exceptionRequests().asScala.foreach(_.enable())
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
          val eventIsConsumed = ev.handle().contains(true)

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

                    doResume = _pauser.handleBreakpoint(ev, prepareForPausing(ev))
                }

              case ev: ClassPrepareEvent =>
                if (hasInitiatedClassScanning) {
                  // A new class, added after we have initiated scanning
                  considerReferenceType(Some(ev.thread()), ev.referenceType())
                } else if (relevantForPostponingClassScan(ev.referenceType())) {
                  // Bump the class counter - when we handle PostponeInitialize, if the class cound has stabilized,
                  // we do a full scan.
                  log.trace(s"New class (${ev.referenceType().name()}), counting it to await full scan when class count has stabilized.")
                  seenClassPrepareRequests += 1
                }
              case ev: ExceptionEvent if pausedData.isEmpty =>
                val exceptionTypeName = ev.exception().referenceType().name()
                val isECMAException = exceptionTypeName == NIR_ECMAException
                if (isECMAException) {
                  val pd = prepareForPausing(ev)
                  maybeEmitErrorEvent(pd)
                  doResume = _pauser.handleBreakpoint(ev, pd)
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

  override def scripts: Seq[Script] = _scripts.scripts

  override def findScript(id: ScriptIdentity): Option[Script] = _scripts.byId(id)

  override def events: Observable[ScriptEvent] = new Observable[ScriptEvent] {
    override def subscribe(observer: Observer[ScriptEvent]): Subscription = {
      // Make sure the observer sees that we're initialized
      if (hostInitializationComplete) {
        observer.onNext(InitialInitializationComplete)
      }
      eventSubject.subscribe(observer)
    }
  }

  protected def findBreakableLocation(location: Location): Option[BreakableLocation] =
    _breakableLocations.byLocation(location)

  protected def findActiveBreakpoint(location: Location): Option[ActiveBreakpoint] = {
    findBreakableLocation(location).map(_breakpoints.activeFor)
  }

  protected def findBreakableLocationsAtLine(id: ScriptIdentity, lineNumber: Int): Option[Seq[BreakableLocation]] =
    _breakableLocations.atLine(id, lineNumber)

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

  private def removeAllBreakpoints(): Unit = _breakpoints.disableAll()

  override def reset(): Unit = {
    log.info("Resetting VM...")
    _pauser.pauseOnBreakpoints(false)
    removeAllBreakpoints()
    resume()
  }

  override def removeBreakpointById(id: String): Unit = _breakpoints.disableById(id)

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
            // Now get the current stack frame list and identify the correct target. This is needed since the old
            // stack frame list isn't valid anymore (due to thread resume due to marshalling).
            pd.thread.frames().asScala.find(f => sf.breakableLocation.hasLocation(f.location())) match {
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
                throw new IllegalArgumentException("Unknown stack frame location: " + sf.breakableLocation)
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


