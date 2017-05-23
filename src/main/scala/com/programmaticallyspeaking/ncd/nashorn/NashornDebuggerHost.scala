package com.programmaticallyspeaking.ncd.nashorn

import java.io.FileNotFoundException
import java.util.Collections
import java.util.concurrent.{Executors, ScheduledFuture, TimeUnit}

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType, Undefined}
import com.programmaticallyspeaking.ncd.infra.{DelayedFuture, IdGenerator}
import com.programmaticallyspeaking.ncd.messaging.{Observable, Observer, Subject, Subscription}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.{JSObjectMirror, ScriptObjectMirror}
import com.sun.jdi.event._
import com.sun.jdi.request.{EventRequest, ExceptionRequest, StepRequest}
import com.sun.jdi.{StackFrame => _, _}
import org.slf4s.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
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

  private[NashornDebuggerHost] class PausedData(val thread: ThreadReference, val stackFrames: Seq[StackFrame],
                                                val marshaller: Marshaller, val isAtDebuggerStatement: Boolean) {
    def clearCaches(): Unit = {
      objectPropertiesCache.clear()
      propertyHolderCache.clear()
    }

    /** We assume that we can cache object properties as long as we're in a paused state. Since we're connected to a
      * Java process, an arbitrary Java object may change while in this state, so we only cache JS objects.
      */
    val objectPropertiesCache = mutable.Map[ObjectPropertiesKey, Map[String, ObjectPropertyDescriptor]]()

    val propertyHolderCache = mutable.Map[ObjectId, Option[PropertyHolder]]()
  }

  private[NashornDebuggerHost] case class ObjectPropertiesKey(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean)

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
}

class NashornDebuggerHost(val virtualMachine: VirtualMachine, asyncInvokeOnThis: ((NashornScriptHost) => Any) => Future[Any]) extends NashornScriptHost with Logging {
  import NashornDebuggerHost._
  import com.programmaticallyspeaking.ncd.infra.BetterOption._

  import ExecutionContext.Implicits._
  import scala.collection.JavaConverters._

  private val scriptByPath = mutable.Map[String, Script]()

  private val breakableLocationsByScriptUrl = mutable.Map[String, ListBuffer[BreakableLocation]]()

  private val enabledBreakpoints = mutable.Map[String, ActiveBreakpoint]()

  private val scriptIdGenerator = new IdGenerator("nds")
  private val breakpointIdGenerator = new IdGenerator("ndb")
  private val stackframeIdGenerator = new IdGenerator("ndsf")

  private val eventSubject = Subject.serialized[ScriptEvent]

  private var isInitialized = false

  private val objectDescriptorById = mutable.Map[ObjectId, ObjectDescriptor]()

  private val objectReferencesWithDisabledGC = ListBuffer[ObjectReference]()

  private val objectReferencesWithDisabledGCForTheEntireSession = ListBuffer[ObjectReference]()

  private var infoAboutLastStep: Option[StepLocationInfo] = None

  /**
    * Keeps track of the stack frame location for a given locals object that we have created to host local variable
    * values. This allows us to update local variables for the correct stack frame (based on its location). We cannot
    * store stack frame, because stack frames are invalidates on thread resume, i.e. on code evaluation.
    */
  private val locationForLocals = mutable.Map[ObjectId, Location]()

  private val mappingRegistry: MappingRegistry = (value: Value, valueNode: ComplexNode, extra: Map[String, ValueNode]) => {
    objectDescriptorById += valueNode.objectId -> ObjectDescriptor(Option(value), valueNode, extra)
  }

  private val foundWantedTypes = mutable.Map[String, ClassType]()

  private val scriptTypesWaitingForSource = ListBuffer[ReferenceType]()
  private val scriptTypesToBreakRetryCycleFor = ListBuffer[ReferenceType]()

  // Data that are defined when the VM has paused on a breakpoint or encountered a step event
  private var pausedData: Option[PausedData] = None

  private var objectPropertiesCacheEnabled = true

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
  private var willPauseOnBreakpoints = false

  private var seenClassPrepareRequests = 0
  private var lastSeenClassPrepareRequests = -1L

  private val exceptionRequests = ListBuffer[ExceptionRequest]()

  private var profiling: Option[OngoingProfiling] = None
  private lazy val profilingExecutor = Executors.newSingleThreadScheduledExecutor()

  private var maybeScriptBasedPropertyHolderFactory: Option[ScriptBasedPropertyHolderFactory] = None

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
    breakableLocationsByScriptUrl.get(script.url.toString) match {
      case Some(locations) =>
        locations.groupBy(_.scriptLocation.lineNumber1Based).foreach {
          case (lineNo, locs) if locs.size > 1 =>
            val sortedLocs = locs.sortWith((bl1, bl2) => {
              val bl1Method = bl1.location.method()
              val bl2Method = bl2.location.method()

              if (bl1Method == bl2Method) {
                val line = script.sourceLine(bl1.scriptLocation.lineNumber1Based)
                throw new UnsupportedOperationException(s"Unexpected, multiple locations in the same method: $bl1 and $bl2, for line $line")
              } else {
                // Different methods
                // # = CompilerConstants.NESTED_FUNCTION_SEPARATOR
                val bl1MethodNameParts = bl1Method.name().split("#")
                val bl2MethodNameParts = bl2Method.name().split("#")
                bl2MethodNameParts.startsWith(bl1MethodNameParts)
              }
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
    DelayedFuture(200.milliseconds) {
      asyncInvokeOnThis(_.handleOperation(PostponeInitialize))
    }
  }

  def initialize(): Unit = {
    watchAddedClasses()
    log.debug("Postponing initialization until classes have stabilized")
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

  private def doInitialize(): Unit = {
    val referenceTypes = virtualMachine.allClasses()

    // Go through reference types that exist so far. More may arrive later!
    referenceTypes.asScala.foreach(considerReferenceType(_: ReferenceType, InitialScriptResolveAttempts))
  }

  private def considerInitializationToBeComplete(): Unit = {
    log.info("Host initialization is complete.")
    isInitialized = true
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
    var bc = methodByteCodes(location.codeIndex().toInt).toInt
    if (bc < 0) bc + 256 else bc
  }

  def handleOperation(eventQueueItem: NashornScriptOperation): Unit = eventQueueItem match {
    case NashornEventSet(es) if hasDeathOrDisconnectEvent(es) =>
      signalComplete()
    case NashornEventSet(eventSet) =>
      var doResume = true
      eventSet.asScala.foreach { ev =>
        try {
          ev match {
            case ev: StepEvent =>
              virtualMachine.eventRequestManager().deleteEventRequest(ev.request())
              val bc = byteCodeFromLocation(ev.location())
              if (IlCodesToIgnoreOnStepEvent.contains(bc)) {
                // We most likely hit an "intermediate" location after returning from a function.
                log.trace(s"Skipping step event at ${ev.location()} because byte code is ignored: 0x${bc.toHexString}")
                createEnabledStepOverRequest(ev.thread(), isAtDebuggerStatement = false)
              } else {
                log.trace(s"Considering step event at ${ev.location()} with byte code: 0x${bc.toHexString}")
                doResume = handleBreakpoint(ev)
                if (!doResume) infoAboutLastStep = Some(StepLocationInfo.from(ev))
              }

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
              if (isInitialized) {
                considerReferenceType(ev.referenceType(), InitialScriptResolveAttempts)
              } else {
                seenClassPrepareRequests += 1
              }
            case ev: ExceptionEvent if pausedData.isEmpty =>
              attemptToResolveSourceLessReferenceTypes()

              val isECMAException = ev.exception().referenceType().name() == NIR_ECMAException
              doResume = !isECMAException || handleBreakpoint(ev)

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
    case x@PostponeInitialize =>

      if (lastSeenClassPrepareRequests == seenClassPrepareRequests) doInitialize()
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

  def functionDetails(functionMethod: Method): FunctionDetails = {
    FunctionDetails(functionMethod.name())
  }

  private def boxed(prim: PrimitiveValue, typeName: String, method: String)(implicit thread: ThreadReference): Value = {
    foundWantedTypes.get(typeName) match {
      case Some(aType) =>
        val invoker = Invokers.shared.getStatic(aType)
        invoker.applyDynamic(method)(prim)
      case None => throw new IllegalArgumentException(s"Failed to find the '$typeName' type.")
    }
  }

  /**
    * Performs boxing of a primitive value, e.g. int => Integer
    * @param thread the thread to run boxing methods on
    * @param prim the primitive value to box
    * @return the boxed value
    */
  // TODO: Move this method to some class where it fits better. Marshaller? VirtualMachineExtensions?
  private def boxed(prim: PrimitiveValue)(implicit thread: ThreadReference): Value = prim match {
    case b: BooleanValue => boxed(b, JL_Boolean, "valueOf(Z)Ljava/lang/Boolean;")
    case i: IntegerValue => boxed(i, JL_Integer, "valueOf(I)Ljava/lang/Integer;")
    case l: LongValue =>
      // LongValue is kept for completeness - Nashorn since8 8u91 or something like that doesn't use Long for
      // representing numbers anymore.
      boxed(l, JL_Long, "valueOf(J)Ljava/lang/Long;")
    case d: DoubleValue => boxed(d, JL_Double, "valueOf(D)Ljava/lang/Double;")
    case _ => throw new IllegalArgumentException("Cannot box " + prim)
  }

  private def scopeWithFreeVariables(scopeObject: Value, freeVariables: Map[String, AnyRef])(implicit marshaller: Marshaller): Value = {
    require(scopeObject != null, "Scope object must be non-null")
    implicit val thread = marshaller.thread
    // If there aren't any free variables, we don't need to create a wrapper scope
    if (freeVariables.isEmpty) return scopeObject

    // Just using "{}" returns undefined - don't know why - but "Object.create" works well.
    // Use the passed scope object as prototype object so that scope variables will be seen as well.
    var scopeObjFactory =
      s"""(function() { var obj = Object.create(this);
         |obj['${hiddenPrefix}changes']=[];
         |obj['${hiddenPrefix}resetChanges']=function(){ obj['${hiddenPrefix}changes'].length=0; };
       """.stripMargin

    // Add an accessor property for each free variable. This allows us to track changes to the variables, which is
    // necessary to be able to update local variables later on.
    freeVariables.foreach {
      case (name, _) =>
        scopeObjFactory +=
          s"""Object.defineProperty(obj,'$name',{
             |  get:function() { return this['$hiddenPrefix$name']; },
             |  set:function(v) { this['${hiddenPrefix}changes'].push('$name',v); this['$hiddenPrefix$name']=v; },
             |  enumerable:true
             |});
           """.stripMargin
    }
    scopeObjFactory += "return obj;}).call(this)"

    val anObject = DebuggerSupport_eval_custom(scopeObject, null, scopeObjFactory).asInstanceOf[ObjectReference]
    val mirror = new ScriptObjectMirror(anObject)
    freeVariables.foreach {
      case (name, value) =>
        val valueToPut = value match {
          case prim: PrimitiveValue => boxed(prim)
          case other => other
        }
        mirror.put(hiddenPrefix + name, valueToPut, isStrict = false)
    }

    anObject
  }

  /** Custom version of jdk.nashorn.internal.runtime.DebuggerSupport.eval that makes a difference between a returned
    * exception value and a thrown exception value. If we use the real DebuggerSupport.eval, there's no way to know
    * if an actual Java exception was returned or thrown as a result of an evaluation error.
    *
    * @param thread the thread to use when invoking methods
    * @param thisObject the object to use as `this`. If `null`, the global object will be used.
    * @param scopeObject the object to use as scope. If `null`, the global object will be used.
    * @param code the code to evaluate
    * @return the result of the evaluation. A thrown exception is wrapped in a [[ThrownExceptionReference]] instance.
    */
  private def DebuggerSupport_eval_custom(thisObject: Value, scopeObject: Value, code: String)(implicit thread: ThreadReference): Value = {
    // Based on the following code:
//    static Object eval(ScriptObject scope, Object self, String string, boolean returnException) {
//      Global global = Context.getGlobal();
//      Object initialScope = scope != null?scope:global;
//      Object callThis = self != null?self:global;
//      Context context = global.getContext();
//
//      try {
//        return context.eval((ScriptObject)initialScope, string, callThis, ScriptRuntime.UNDEFINED);
//      } catch (Throwable var9) {
//        return returnException?var9:null;
//      }
//    }
    foundWantedTypes.get(NIR_Context) match {
      case Some(ct: ClassType) =>
        val invoker = Invokers.shared.getStatic(ct)

        val global = invoker.getGlobal()
        val globalInvoker = Invokers.shared.getDynamic(global.asInstanceOf[ObjectReference])
        val initialScope = if (scopeObject != null) scopeObject else global
        val callThis = if (thisObject != null) thisObject else global
        val context = globalInvoker.getContext()
        val contextInvoker = Invokers.shared.getDynamic(context.asInstanceOf[ObjectReference])

        try {
          val codeWithMarker = s"""'$EvaluatedCodeMarker';$code"""
          contextInvoker.eval(initialScope, codeWithMarker, callThis, null)
        } catch {
          case ex: InvocationFailedException =>
            new ThrownExceptionReference(virtualMachine, ex.exceptionReference)
        }

      case _ =>
        throw new IllegalStateException("The Context type wasn't found, cannot evaluate code.")
    }
  }

  // Determines if the location is in ScriptRuntime.DEBUGGER.
  private def isDebuggerStatementLocation(loc: Location) =
    loc.declaringType().name() == NIR_ScriptRuntime && loc.method().name() == ScriptRuntime_DEBUGGER

  private def scopeTypeFromValueType(value: Value): ScopeType = {
    val typeName = value.`type`().name()
    // jdk.nashorn.internal.objects.Global
    if (typeName.endsWith(".Global"))
      return ScopeType.Global
    // jdk.nashorn.internal.runtime.WithObject
    if (typeName.endsWith(".WithObject"))
      return ScopeType.With
    ScopeType.Closure
  }

  private def prototypeOf(value: Value)(implicit thread: ThreadReference): Option[Value] = value match {
    case ref: ObjectReference =>
      val invoker = Invokers.shared.getDynamic(ref)
      val maybeProto = Option(invoker.getProto())
      // Prototype of Global is jdk.nashorn.internal.objects.NativeObject$Prototype - ignore it
      if (maybeProto.exists(_.`type`().name().endsWith("$Prototype"))) None
      else maybeProto
    case _ => None
  }

  private def prototypeChain(value: Value)(implicit thread: ThreadReference): Seq[Value] = prototypeOf(value) match {
    case Some(proto) => Seq(proto) ++ prototypeChain(proto)
    case None => Seq.empty
  }

  private def createScopeChain(marshaller: Marshaller, originalScopeValue: Option[Value], thisValue: Value, marshalledThisNode: ValueNode, localNode: Option[ObjectNode]): Seq[Scope] = {
    implicit val thread: ThreadReference = marshaller.thread
    // Note: I tried to mimic how Chrome reports scopes, but it's a bit difficult. For example, if the current scope
    // is a 'with' scope, there is no way (that I know of) to determine if we're in a function (IIFE) inside a with
    // block or if we're inside a with block inside a function.
    def toScope(v: Value) = Scope(marshaller.marshal(v), scopeTypeFromValueType(v))
    def findGlobalScope(): Option[Scope] = {
      if (scopeTypeFromValueType(thisValue) == ScopeType.Global) {
        // this == global, so no need to call Context.getGlobal().
        Some(Scope(marshalledThisNode, ScopeType.Global))
      } else {
        foundWantedTypes.get(NIR_Context) match {
          case Some(context) =>
            val invoker = Invokers.shared.getStatic(context)
            val global = invoker.getGlobal()
            Option(global).map(toScope)
          case None =>
            // No global found :-(
            None
        }
      }
    }

    val scopeChain = ListBuffer[Scope]()

    // If we have locals, add a local scope
    localNode.foreach(scopeChain += Scope(_, ScopeType.Local))

    originalScopeValue.map(v => (v, toScope(v))) match {
      case Some((_, s)) if s.scopeType == ScopeType.Global =>
        // If the current scope is the global scope, add it but don't follow the prototype chain since it's unnecessary.
        scopeChain += s
      case Some((v, s)) =>
        // Add the scope and all its parent scopes
        scopeChain += s
        scopeChain ++= prototypeChain(v).map(toScope)
      case None =>
        // noop
    }

    // Make sure we have a global scope lsat!
    if (!scopeChain.exists(_.scopeType == ScopeType.Global)) {
      scopeChain ++= findGlobalScope()
    }

    scopeChain
  }

  private def buildStackFramesSequence(perStackFrame: Seq[(Map[String, Value], Location)])(implicit marshaller: Marshaller): Seq[StackFrameHolder] = {
    implicit val thread: ThreadReference = marshaller.thread
    perStackFrame.map {
      case (values, location) =>
        val functionMethod = location.method()

        // Generate an ID for the stack frame so that we can find it later when asked to evaluate code for a
        // particular stack frame.
        val stackframeId = stackframeIdGenerator.next

        // ":this" should always be present, but a function that doesn't capture anything may lack a ":scope" variable
        values.get(":this") match {
          case Some(originalThis) =>
            val originalScope = values.get(":scope")

            // Variables that don't start with ":" are locals
            val localValues = values.filter(e => !e._1.startsWith(":")) // for use in evaluateCodeOnFrame

            val thisObj = marshaller.marshal(originalThis)

            // If needed, create a scope object to hold the local variables as "free" variables - so that evaluated
            // code can refer to them.
            // If we don't have :scope, use :this - it's used as a parent object for the created scope object.
            val localScope = scopeWithFreeVariables(originalScope.getOrElse(originalThis), localValues)

            // Create an artificial object node to hold the locals. Note that the object ID must be unique per stack
            // since we store object nodes in a map.
            val locals = localValues.map(e => e._1 -> marshaller.marshal(e._2))
            val localNode = if (locals.nonEmpty) {
              val objectId = ObjectId(localScopeObjectIdPrefix + stackframeId)
              val node = ObjectNode("Object", objectId)

              // Note: Don't register locals as extra properties, since they will shadow the real properties on the
              // local scope object.
              mappingRegistry.register(localScope, node, Map.empty)

              // Track location (of the stack frame), so that we can update locals later on
              locationForLocals += objectId -> location

              Some(node)
            } else None

            val scopeChain = createScopeChain(marshaller, originalScope, originalThis, thisObj, localNode)

            def evaluateCodeOnFrame: CodeEvaluator = {
              case (code, namedValues) =>
                // Create a scope object for the extra variables to use during evaluation, if any.
                val scopeToUse = scopeWithFreeVariables(localScope, namedValues)

                try {
                  val ret = DebuggerSupport_eval_custom(originalThis, scopeToUse, code)
                  marshaller.marshal(ret) match {
                    case SimpleValue(str: String) if str == EvaluatedCodeMarker =>
                      // A non-expression statements such as "var x = 42" causes the evaluation marker to leak as an
                      // expression result. We suppress it here!
                      SimpleValue(Undefined)
                    case other => other
                  }
                } catch {
                  case ex: Exception =>
                    // Don't log this at error level, because the error may be "ok". For example, if the user hovers over
                    // a property of a variable that contains undefined then DevTools will ask about the property value
                    // with silent errors, and when getting the value blows up we shouldn't be noisy!
                    log.debug("Code evaluation failed.", ex)
                    throw ex
                }
            }

            try {
              findActiveBreakpoint(location).map(ab => StackFrameImpl(stackframeId, thisObj, scopeChain, ab, evaluateCodeOnFrame, functionDetails(functionMethod))) match {
                case Some(sf) => StackFrameHolder(Some(sf))
                case None =>
                  log.warn(s"Won't create a stack frame for location ($location) since we don't recognize it.")
                  StackFrameHolder(None)
              }
            } catch {
              case ex: AbsentInformationException =>
                log.warn(s"Won't create a stack frame for location ($location) since there's no source information.")
                StackFrameHolder(None)
            }
          case _ =>
            StackFrameHolder(None, Some(location))
        }

    }
  }

  private def captureStackFrames(thread: ThreadReference)(implicit marshaller: Marshaller): Seq[StackFrameHolder] = {
    // Get all Values FIRST, before marshalling. This is because marshalling requires us to call methods, which
    // will temporarily resume threads, which causes the stack frames to become invalid.
    val perStackFrame = thread.frames().asScala.map { sf =>
      // In the step tests, I get JDWP error 35 (INVALID SLOT) for ':return' and since we don't use it, leave it for
      // now. If we need it, we can get it separately.
      val variables = Try(sf.visibleVariables()).getOrElse(Collections.emptyList()).asScala.filter(_.name() != ":return").asJava
      try {
        val values = sf.getValues(variables).asScala.map(e => e._1.name() -> e._2)
        (values.toMap, sf.location())
      } catch {
        case JDWP_ERR_INVALID_SLOT(_) =>
          val scalaVars = variables.asScala
          val vars = scalaVars.map(_.name()).mkString(", ")
          log.warn(s"INVALID_SLOT error for: $vars")

          // Get variable values one by one, and ignore the ones that result in INVALID_SLOT.
          // Another idea is to generate a fake value, something like "@@ERR:INVALID_SLOT". Good?
          val entries = scalaVars.flatMap(v => {
            try Some(v.name() -> sf.getValue(v)) catch {
              case JDWP_ERR_INVALID_SLOT(_) =>
                log.warn(s"INVALID_SLOT error for variable '${v.name()}', ignoring")
                None
            }
          })
          (entries.toMap, sf.location())
      }
    }

    // Second pass, marshal
    buildStackFramesSequence(perStackFrame)
  }

  private def enableGarbageCollectionWhereDisabled(): Unit = {
    objectReferencesWithDisabledGC.foreach(_.enableCollection())
    objectReferencesWithDisabledGC.clear()
  }

  private def disableGarbageCollectionFor(value: Value, entireSession: Boolean = false): Unit = value match {
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

  private def handleBreakpoint(ev: LocatableEvent): Boolean = {
    // Resume right away if we're not pausing on breakpoints
    if (!willPauseOnBreakpoints) return true

    // Log at debug level because we get noise due to exception requests.
    log.debug(s"A breakpoint was hit at location ${ev.location()} in thread ${ev.thread().name()}")
    implicit val thread = ev.thread()

    // Start with a fresh object registry
    objectDescriptorById.clear()

    locationForLocals.clear()

    // Shared marshaller
    implicit val marshaller = createMarshaller()

    val stackFrames = captureStackFrames(thread)
    stackFrames.headOption match {
      case Some(holder) if holder.stackFrame.isEmpty && !holder.mayBeAtSpecialStatement =>
        // First/top stack frame doesn't belong to a script. Resume!
        log.debug(s"Ignoring breakpoint at ${ev.location()} because it doesn't belong to a script.")
        true
      case Some(holder) =>
        if (holder.isAtDebuggerStatement) log.debug("Breakpoint is at JavaScript 'debugger' statement")
        val didPause = doPause(thread, stackFrames.flatMap(_.stackFrame), holder.isAtDebuggerStatement)
        // Resume will be controlled externally
        !didPause // false
      case None =>
        // Hm, no stack frames at all... Resume!
        log.debug(s"Ignoring breakpoint at ${ev.location()} because no stack frames were found at all.")
        true
    }
  }

  private def doPause(thread: ThreadReference, stackFrames: Seq[StackFrame], atDebugger: Boolean)(implicit marshaller: Marshaller): Boolean = {
    stackFrames.headOption.collect { case sf: StackFrameImpl => sf } match {
      case Some(topStackFrame) =>
        val breakpoint = topStackFrame.breakpoint

        // Check condition if we have one. We cannot do this until now (which means that a conditional breakpoint
        // will be slow) because we need stack frames and locals to be setup for code evaluation.
        val conditionIsTrue = topStackFrame.activeBreakpoint.condition match {
          case Some(c) =>
            topStackFrame.eval(c, Map.empty) match {
              case SimpleValue(true) => true
              case SimpleValue(false) =>
                log.trace(s"Not pausing on breakpoint ${breakpoint.breakpointId} in script ${breakpoint.scriptId} since the condition ($c) evaluated to false.")
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

          scriptById(breakpoint.scriptId).foreach { s =>
            val line = s.sourceLine(breakpoint.location.lineNumber1Based).getOrElse("<unknown line>")
            log.info(s"Pausing at ${s.url}:${breakpoint.location.lineNumber1Based}: $line")
          }

          val hitBreakpoint = HitBreakpoint(stackFrames)
          emitEvent(hitBreakpoint)
        }
        conditionIsTrue
      case None =>
        throw new IllegalStateException("Unexpected - no stack frame head")
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

  override def scriptById(id: String): Option[Script] = scripts.find(_.id == id) //TODO: make more efficient

  override def events: Observable[ScriptEvent] = new Observable[ScriptEvent] {
    override def subscribe(observer: Observer[ScriptEvent]): Subscription = {
      // Make sure the observer sees that we're initialized
      if (isInitialized) {
        observer.onNext(InitialInitializationComplete)
      }
      eventSubject.subscribe(observer)
    }
  }

  override def setBreakpoint(scriptUri: String, location: ScriptLocation, condition: Option[String]): Option[Breakpoint] = {
    findBreakableLocationsAtLine(scriptUri, location.lineNumber1Based) match {
      case Some(bls) =>
        // If we have a column number, find exactly that location. Otherwise grab all locations
        val candidates = location.columnNumber1Based match {
          case Some(col) => bls.filter(_.scriptLocation.columnNumber1Based.contains(col))
          case None => bls
        }
        if (candidates.nonEmpty) {
          val newId = breakpointIdGenerator.next
          val conditionDescription = condition.map(c => s" with condition ($c)").getOrElse("")
          log.info(s"Setting a breakpoint with ID $newId for location(s) ${candidates.mkString(", ")} in $scriptUri$conditionDescription")

          // Force boolean and handle that the condition contains a trailing comment
          val wrapper = condition.map(c =>
            s"""!!(function() {
               |return $c
               |})()
           """.stripMargin)

          val activeBp = ActiveBreakpoint(newId, candidates, wrapper)
          activeBp.enable()
          enabledBreakpoints += (activeBp.id -> activeBp)
          Some(activeBp.toBreakpoint)
        } else None

      case None =>
        log.trace(s"No breakable locations found for script $scriptUri at line ${location.lineNumber1Based}")
        None
    }
  }

  private def findBreakableLocation(location: Location): Option[BreakableLocation] = {
    scriptByPath.get(scriptPathFromLocation(location)).flatMap { script =>
      // We cannot compare locations directly because the passed-in Location may have a code index that is
      // different from the one stored in a BreakableLocation - so do a line comparison.
      findBreakableLocationsAtLine(script.url.toString, location.lineNumber()).flatMap(_.find(bl => sameMethodAndLine(bl.location, location)))
    }
  }

  private def sameMethodAndLine(l1: Location, l2: Location): Boolean = {
    l1.method() == l2.method() && l1.lineNumber() == l2.lineNumber()
  }

  private def findActiveBreakpoint(location: Location): Option[ActiveBreakpoint] = {
    findBreakableLocation(location).map { bl =>
      enabledBreakpoints.values.find(_.contains(bl)) match {
        case Some(ab) => ab // existing active breakpoint
        case None => ActiveBreakpoint(breakpointIdGenerator.next, Seq(bl), None) // temporary breakpoint (e.g. debugger statement)
      }
    }
  }

  private def findBreakableLocationsAtLine(scriptUrl: String, lineNumber: Int): Option[Seq[BreakableLocation]] = {
    breakableLocationsByScriptUrl.get(scriptUrl).map { breakableLocations =>
      breakableLocations.filter(_.scriptLocation.lineNumber1Based == lineNumber)
    }
  }

  private def resumeWhenPaused(): Unit = pausedData match {
    case Some(data) =>
      log.info("Resuming virtual machine")
      enableGarbageCollectionWhereDisabled()
      virtualMachine.resume()
      pausedData = None
      objectDescriptorById.clear() // only valid when paused
      data.clearCaches()
      emitEvent(Resumed)
    case None =>
      log.debug("Ignoring resume request when not paused (no pause data).")
  }

  override def resume(): Unit = {
    resumeWhenPaused()
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

  override def step(stepType: StepType): Unit = pausedData match {
    case Some(pd) =>
      log.info(s"Stepping with type $stepType")

      // Note that we don't issue normal step requests to the remove VM, because a script line != a Java line, so if we
      // were to request step out, for example, we might end up in some method that acts as a script bridge.
      stepType match {
        case StepInto =>
          createEnabledStepIntoRequest(pd.thread)
        case StepOver =>
          createEnabledStepOverRequest(pd.thread, pd.isAtDebuggerStatement)
        case StepOut =>
          createEnabledStepOutRequest(pd.thread, pd.isAtDebuggerStatement)
      }

      resumeWhenPaused()
    case None =>
      throw new IllegalStateException("A breakpoint must be active for stepping to work")
  }

  private def createEnabledStepRequest(thread: ThreadReference, depth: Int, count: Int): Unit = {
    val sr = virtualMachine.eventRequestManager().createStepRequest(thread, StepRequest.STEP_LINE, depth)
    sr.addClassFilter(StepRequestClassFilter)
    if (count > 0) sr.addCountFilter(count)
    sr.enable()
  }

  private def createEnabledStepIntoRequest(thread: ThreadReference): Unit = {
    createEnabledStepRequest(thread, StepRequest.STEP_INTO, -1)
  }

  private def createEnabledStepOverRequest(thread: ThreadReference, isAtDebuggerStatement: Boolean): Unit = {
    createEnabledStepRequest(thread, StepRequest.STEP_OVER, if (isAtDebuggerStatement) 2 else -1)
  }

  private def createEnabledStepOutRequest(thread: ThreadReference, isAtDebuggerStatement: Boolean): Unit = {
    //TODO: Why 3 and not 2??
    createEnabledStepRequest(thread, StepRequest.STEP_OUT, if (isAtDebuggerStatement) 3 else -1)
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

  private def updateChangedLocals(sf: StackFrameImpl, namedValues: Map[String, AnyRef], namedObjects: Map[String, ObjectId])(implicit marshaller: Marshaller): Unit = {
    def jdiStackFrameForObject(id: ObjectId) = locationForLocals.get(id).flatMap(jdiStackFrameFromLocation(marshaller.thread))

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
                    Try(Option(jdiStackFrame.visibleVariableByName(name))).map(_.foreach(jdiStackFrame.setValue(_, newValue))) match {
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

  private def jdiStackFrameFromLocation(thread: ThreadReference)(loc: Location) =
    thread.frames().asScala.find(_.location() == loc)

  private def findStackFrame(pausedData: PausedData, id: String): Option[StackFrame] = {
    if (id == "$top") return pausedData.stackFrames.headOption
    pausedData.stackFrames.find(_.id == id)
  }

  override def pauseOnBreakpoints(): Unit = {
    log.info("Will pause on breakpoints")
    willPauseOnBreakpoints = true
  }

  override def ignoreBreakpoints(): Unit = {
    log.info("Will ignore breakpoints")
    willPauseOnBreakpoints = false
  }

  override def pauseOnExceptions(pauseType: ExceptionPauseType): Unit = {
    val erm = virtualMachine.eventRequestManager()

    // Clear all first, simpler than trying to keep in sync
    erm.deleteEventRequests(exceptionRequests.asJava)
    exceptionRequests.clear()

    val pauseOnCaught = pauseType == ExceptionPauseType.Caught || pauseType == ExceptionPauseType.All
    // Note that uncaught is currently untested since our test setup doesn't really allow it.
    val pauseOnUncaught = pauseType == ExceptionPauseType.Uncaught || pauseType == ExceptionPauseType.All

    if (pauseOnCaught || pauseOnUncaught) {
      log.info(s"Will pause on exceptions (caught=$pauseOnCaught, uncaught=$pauseOnUncaught)")
      val request = erm.createExceptionRequest(null, pauseOnCaught, pauseOnUncaught)
      request.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD) // TODO: Duplicate code
      request.setEnabled(true)
      exceptionRequests += request
    } else {
      log.info("Won't pause on exceptions")
    }
  }

  private def accessorToDataForLocals(objectId: ObjectId)(prop: (String, ObjectPropertyDescriptor)): (String, ObjectPropertyDescriptor) = {
    if (objectId.id.startsWith(localScopeObjectIdPrefix) && prop._2.descriptorType == PropertyDescriptorType.Accessor) {
      val desc = prop._2
      // Yeah, getting to the getter ID is ugly, but it must work since we know we have an accessor property.
      val getterId = desc.getter.get.asInstanceOf[ComplexNode].objectId
      evaluateOnStackFrame("$top", "fun.call(owner)", Map("fun" -> getterId, "owner" -> objectId)) match {
        case Success(vn) =>
          val newDescriptor = desc.copy(descriptorType = PropertyDescriptorType.Data,
            getter = None,
            setter = None,
            value = Some(vn),
            isWritable = desc.setter.isDefined
          )
          (prop._1, newDescriptor)

        case Failure(t) =>
          log.error(s"Failed to invoke the getter for ${prop._1} on $objectId", t)
          prop
      }
    } else prop
  }

  private def createPropertyHolder(objectId: ObjectId, objectDescriptor: ObjectDescriptor, includeProto: Boolean)(implicit marshaller: Marshaller): Option[PropertyHolder] = {
    val cache = pausedData.get.propertyHolderCache
    implicit val thread = marshaller.thread

    def scriptObjectHolder(ref: ObjectReference) = {
      var blacklistParts = Seq(hiddenPrefixEscapedForUseInJavaScriptRegExp + ".+")
      if (!includeProto) blacklistParts +:= "__proto__"
      val propBlacklistRegex = blacklistParts.map("(" + _ + ")").mkString("^", "|", "$")
      val factory = scriptBasedPropertyHolderFactory()
      val holder = factory.create(ref, propBlacklistRegex, isNative = true)
      new PropertyHolder {
        override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = {
          holder.properties(onlyOwn, onlyAccessors).map(accessorToDataForLocals(objectId))
        }
      }
    }

    //TODO: We can use scriptObjectHolder for ScriptObjectMirror also, but when do we one of those?
    cache.getOrElseUpdate(objectId, {
      objectDescriptor.native collect {
        case ref: ObjectReference if marshaller.isScriptObject(ref) =>
          scriptObjectHolder(ref)
        case ref: ObjectReference if marshaller.isJSObject(ref) =>
          new JSObjectMirror(ref)
        case ref: ArrayReference =>
          new ArrayPropertyHolder(ref)
        case obj: ObjectReference if marshaller.isHashtable(obj) =>
          val factory = scriptBasedPropertyHolderFactory()
          factory.create(obj, "", isNative = false)
        case obj: ObjectReference =>
          new ArbitraryObjectPropertyHolder(obj)
      }
    })
  }

  private def scriptBasedPropertyHolderFactory()(implicit threadReference: ThreadReference): ScriptBasedPropertyHolderFactory = {
    maybeScriptBasedPropertyHolderFactory match {
      case Some(f) => f
      case None =>
        val codeEval: (String) => Value = src => {
          val v = DebuggerSupport_eval_custom(null, null, src)
          disableGarbageCollectionFor(v, entireSession = true)
          v
        }
        val funExec: (Value, Seq[Any]) => Value = (fun, args) => {
          foundWantedTypes.get(NIR_ScriptRuntime) match {
            case Some(ct) =>
              val invoker = Invokers.shared.getStatic(ct)
              // Object apply(ScriptFunction target, Object self, Object... args) {
              invoker.apply(fun, null, args.toArray)

            case None => throw new RuntimeException("ScriptRuntime wasn't found")
          }
        }
        val f = new ScriptBasedPropertyHolderFactory(codeEval, funExec)
        maybeScriptBasedPropertyHolderFactory = Some(f)
        f
    }
  }

  override def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = pausedData match {
    case Some(pd) =>
      implicit val marshaller = pd.marshaller
      implicit val thread = marshaller.thread

      val scopeObjectIds: Seq[ObjectId] = pd.stackFrames.flatMap(_.scopeChain).map(_.value).collect{case o: ObjectNode => o.objectId}
      val isScopeObject = scopeObjectIds.contains(objectId)

      // For scope objects, DevTools passes onlyOwn==false, but manual testing shows that Chrome itself only returns
      // the scope-own properties. Perhaps scopes aren't prototypically related in Chrome?
      val actualOnlyOwn = onlyOwn || isScopeObject

      // We're not interested in the __proto__ property for scope objects.
      val includeProto = !isScopeObject

      objectDescriptorById.get(objectId) match {
        case Some(desc: ObjectDescriptor) =>
          // Get object properties, via a cache.
          val cacheKey = ObjectPropertiesKey(objectId, actualOnlyOwn, onlyAccessors)
          def getProperties = createPropertyHolder(objectId, desc, includeProto).map(_.properties(actualOnlyOwn, onlyAccessors)).getOrElse(Map.empty)
          val objectProperties = if (objectPropertiesCacheEnabled)
            pausedData.get.objectPropertiesCache.getOrElseUpdate(cacheKey, getProperties)
          else getProperties

          // In addition, the node may contain extra entries that typically do not come from Nashorn. One example is
          // the Java stack we add if we detect a Java exception.
          val extraProps = desc.extras.map(e => {
            e._1 -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true, isWritable = false,
              isOwn = true, Some(e._2), None, None)
          })

          // Combine the two maps
          objectProperties ++ extraProps

        case None =>
          log.warn (s"Unknown object ($objectId), cannot get properties")
          Map.empty
      }
    case None =>
      throw new IllegalStateException("Property extraction can only be done in a paused state.")
  }

  override def getBreakpointLocations(scriptId: String, from: ScriptLocation, to: Option[ScriptLocation]): Seq[ScriptLocation] = {
    scriptById(scriptId).flatMap(script => breakableLocationsByScriptUrl.get(script.url.toString)) match {
      case Some(locations) =>
        // Get hold of all script locations we know of, but since Nashorn/Java doesn't report column number, we
        // a) ignore the column number
        // b) may end up with multiple ones with the same line number
        val candidates = locations.map(_.scriptLocation).filter { sloc =>
          sloc.lineNumber1Based >= from.lineNumber1Based && to.forall(sloc.lineNumber1Based < _.lineNumber1Based)
        }

        //TODO: Update doc
        // Filter so that we end up with one location per line, max. Since ScriptLocation is a case class and all
        // column numbers on the same line will be the same (again, since Nashorn/Java doesn't report column numbers),
        // it's sufficient to get the unique locations.
        candidates.distinct.sortBy(_.columnNumber1Based)

      case None => throw new IllegalArgumentException("Unknown script ID: " + scriptId)
    }
  }

  override def restartStackFrame(stackFrameId: String): Seq[StackFrame] = {
    pausedData match {
      case Some(pd) =>
        log.warn(s"Request to restart stack frame $stackFrameId. Note that depending on the Java version of the target, this may cause the target to crash.")
        // Get the Location of the stack frame to pop
        pd.stackFrames.find(_.id == stackFrameId) match {
          case Some(sf: StackFrameImpl) =>
            val location = sf.activeBreakpoint.firstBreakableLocation.location // ew

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
          case _ => throw new IllegalArgumentException("Unknown stack frame ID: " + stackFrameId)
        }

      case None =>
        throw new IllegalStateException("Frame restart can only be done in a paused state.")
    }
  }

  case class StackFrameImpl(id: String, thisObj: ValueNode, scopeChain: Seq[Scope],
                            activeBreakpoint: ActiveBreakpoint,
                            eval: CodeEvaluator,
                            functionDetails: FunctionDetails) extends StackFrame {
    val breakpoint = activeBreakpoint.toBreakpoint
  }

  case class StackFrameHolder(stackFrame: Option[StackFrame], location: Option[Location] = None) {
    val mayBeAtSpecialStatement = location.isDefined
    val isAtDebuggerStatement = location.exists(isDebuggerStatementLocation)
  }

  override def startProfiling(samplingInterval: FiniteDuration): Unit = {
    require(profiling.isEmpty, "Only one profiling can be ongoing at a time")

    log.info(s"Starting profiling with sampling interval ${samplingInterval.toMicros} microseconds")
    val now = System.nanoTime()
    val samples = ListBuffer[Sample]()
    val collect: Runnable = () => {
      val f = asyncInvokeOnThis(_.collectProfilingSample())
      // To prevent the profiling executor from just scheduling a lot of async collections, we must wait for each
      // collection to complete.
      try Await.result(f, 1.second) catch {
        case NonFatal(t) => log.error("Sample collection failed", t)
      }
    }

    val ss = profilingExecutor.scheduleAtFixedRate(collect, 0, samplingInterval.toMicros, TimeUnit.MICROSECONDS)
    profiling = Some(OngoingProfiling(ss, samples, now))
  }

  override def stopProfiling(): ProfilingData = profiling match {
    case Some(ongoing) =>
      val now = System.nanoTime()

      // Copy the samples right away, since the samples list is mutable and may change before the scheduled job stops.
      val samplesCopy = Seq(ongoing.samples: _*)

      log.info(s"Stopping profiling, collected ${samplesCopy.size} samples")
      ongoing.schedule.cancel(false)

      profiling = None
      ProfilingData(samplesCopy, ongoing.startNanos, now)
    case None => throw new IllegalArgumentException("No ongoing profiling")
  }

  private def buildStackFramesSequenceForProfiling(locations: Seq[Location], thread: ThreadReference): Seq[StackFrameHolder] = {
    locations.map { location =>
      val functionMethod = location.method()
      val stackframeId = stackframeIdGenerator.next

      try {
        //TODO: Should we include a native frame, e.g. inside NativeInt8Array?
        findActiveBreakpoint(location).map(ab => StackFrameImpl(stackframeId, null, Seq.empty, ab, null, functionDetails(functionMethod))) match {
          case Some(sf) => StackFrameHolder(Some(sf))
          case None => StackFrameHolder(None)
        }
      } catch {
        case _: AbsentInformationException => StackFrameHolder(None)
      }
    }
  }

  def collectProfilingSample(): Unit = profiling match {
    case Some(ongoing) =>
      val now = System.nanoTime()
      virtualMachine.suspend()
      try {
        val relevantThreads = virtualMachine.allThreads().asScala.filterNot(isInfrastructureThread).filter(isRunningThread)

        val stackFrameListPerThread = relevantThreads.map { thread =>
          val locations = thread.frames().asScala.map(_.location())
          buildStackFramesSequenceForProfiling(locations, thread).flatMap(_.stackFrame)
        }.filter(_.nonEmpty)

        val samples = if (stackFrameListPerThread.nonEmpty) {
          stackFrameListPerThread.map(Sample(now, _, SampleType.Script))
        } else if (relevantThreads.nonEmpty) {
          // Non-script threads running
          Seq(Sample(now, Seq.empty, SampleType.Java))
        } else {
          // No running relevant threads, so the target is idle
          Seq(Sample(now, Seq.empty, SampleType.Idle))
        }

        // TODO: One sample for all threads? Or one per thread?
        ongoing.samples ++= samples

      } finally virtualMachine.resume()

    case None =>
      log.warn("Cannot collect a profiling sample - no ongoing profiling!")
  }

  override def disableObjectPropertiesCache(): Unit = objectPropertiesCacheEnabled = false
}

case class OngoingProfiling(schedule: ScheduledFuture[_], samples: ListBuffer[Sample], startNanos: Long)

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
