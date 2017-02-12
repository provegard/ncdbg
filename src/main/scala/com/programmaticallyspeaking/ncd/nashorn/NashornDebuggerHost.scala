package com.programmaticallyspeaking.ncd.nashorn

import java.io.{File, FileNotFoundException}
import java.net.URI
import java.util.Collections

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType, Undefined}
import com.programmaticallyspeaking.ncd.infra.{DelayedFuture, IdGenerator}
import com.programmaticallyspeaking.ncd.messaging.{Observable, Subject}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.{JSObjectMirror, ScriptObjectMirror}
import com.sun.jdi.event._
import com.sun.jdi.request.EventRequest
import com.sun.jdi.{StackFrame => _, _}
import org.slf4s.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


object NashornDebuggerHost {
  import scala.collection.JavaConverters._

  val InitialScriptResolveAttempts = 5

  val NIR_DebuggerSupport = "jdk.nashorn.internal.runtime.DebuggerSupport"
  val NIR_ScriptRuntime = "jdk.nashorn.internal.runtime.ScriptRuntime"
  val JL_Boolean = "java.lang.Boolean"
  val JL_Integer = "java.lang.Integer"
  val JL_Long = "java.lang.Long"
  val JL_Double = "java.lang.Double"

  // The name of the DEBUGGER method in the ScriptRuntime class
  val ScriptRuntime_DEBUGGER = "DEBUGGER"

  val wantedTypes = Set(
    NIR_DebuggerSupport,
    NIR_ScriptRuntime,
    JL_Boolean,
    JL_Integer,
    JL_Long,
    JL_Double
  )

  type CodeEvaluator = (String, Map[String, AnyRef]) => ValueNode

  def optionToEither[A](opt: Option[A], msg: => String): Either[String, A] = opt match {
    case Some(a) => Right(a)
    case None => Left(msg)
  }

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
}

class NashornDebuggerHost(val virtualMachine: VirtualMachine, asyncInvokeOnThis: ((NashornScriptHost) => Unit) => Unit) extends NashornScriptHost with Logging {
  import NashornDebuggerHost._

  import ExecutionContext.Implicits._
  import scala.collection.JavaConverters._

  private val scriptByPath = mutable.Map[String, Script]()

  private val breakableLocationsByScriptUri = mutable.Map[String, ListBuffer[BreakableLocation]]()
  private val enabledBreakpoints = mutable.Map[String, BreakableLocation]()

  private val scriptIdGenerator = new IdGenerator("nds")
  private val breakpointIdGenerator = new IdGenerator("ndb")
  private val stackframeIdGenerator = new IdGenerator("ndsf")

  private val eventSubject = Subject.serialized[ScriptEvent]

  private var isInitialized = false

  private val objectPairById = mutable.Map[ObjectId, (Option[Value], ComplexNode, Map[String, ValueNode])]()

  private val mappingRegistry: MappingRegistry = (value: Value, valueNode: ComplexNode, extra: Map[String, ValueNode]) => {
    objectPairById += valueNode.objectId -> (Option(value), valueNode, extra)
  }

  private val foundWantedTypes = mutable.Map[String, ClassType]()

  private val scriptTypesWaitingForSource = ListBuffer[ReferenceType]()
  private val scriptTypesToBreakRetryCycleFor = ListBuffer[ReferenceType]()

  // Data that are defined when the VM has paused on a breakpoint or encountered a step event
  private var pausedData: Option[PausedData] = None

  /**
    * By default, we don't pause when a breakpoint is hit. This is important since we add a fixed breakpoint for
    * JS 'debugger' statements, and we don't want that to pause the VM when a debugger hasn't attached yet.
    */
  private var willPauseOnBreakpoints = false

  private var seenClassPrepareRequests = 0
  private var lastSeenClassPrepareRequests = -1L

  private def addBreakableLocations(script: Script, breakableLocations: Seq[BreakableLocation]): Unit = {
    breakableLocationsByScriptUri.getOrElseUpdate(script.uri, ListBuffer.empty) ++= breakableLocations
  }

  private def enableBreakingAtDebuggerStatement(/*scriptRuntime: ClassType*/): Unit = {
    val debuggerLoc = for {
      scriptRuntime <- optionToEither(foundWantedTypes.get(NIR_ScriptRuntime), "no ScriptRuntime type found")
      debuggerMethod <- optionToEither(scriptRuntime.methodsByName(ScriptRuntime_DEBUGGER).asScala.headOption, "ScriptRuntime.DEBUGGER method not found")
      location <- optionToEither(debuggerMethod.allLineLocations().asScala.headOption, "no line location found in ScriptRuntime.DEBUGGER")
    } yield location

    debuggerLoc match {
      case Right(location) =>
        log.info("Enabling automatic breaking at JavaScript 'debugger' statements")
        // TODO: BreakableLocation also does this. Reuse code!
        val br = virtualMachine.eventRequestManager().createBreakpointRequest(location)
        br.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
        br.setEnabled(true)
      case Left(msg) =>
        log.warn(s"Won't be able to break at JavaScript 'debugger' statements because $msg")
    }
  }

  private def considerReferenceType(refType: ReferenceType, attemptsLeft: Int): Option[Script] = {
    if (attemptsLeft == 0) return None

    val className = refType.name()

    if (wantedTypes.contains(className)) {
      refType match {
        case ct: ClassType =>
          log.debug(s"Found the $className type")
          foundWantedTypes += className -> ct
        case other =>
          log.warn(s"Found the $className type but it's a ${other.getClass.getName} rather than a ClassType")
      }
      None
    } else if (className.startsWith("jdk.nashorn.internal.scripts.Script$")) {
      // This is a compiled Nashorn script class.
      log.debug(s"Script reference type: ${refType.name} ($attemptsLeft attempts left)")

      val locations = refType.allLineLocations().asScala
      locations.headOption match {
        case Some(firstLocation) =>
          val scriptPath = scriptPathFromLocation(firstLocation)

          val triedScript: Try[Either[String, Script]] = Try {
            if (firstLocation.sourceName() == "<eval>") {
              shamelesslyExtractEvalSourceFromPrivatePlaces(refType) match {
                case Some(src) => Right(getOrAddEvalScript(scriptPath, src))
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
            } else {
              // Create and add the Script object. Note that we may hit the same script multiple times, e.g. if the
              // script contains inner functions, which are compiled into separate classes.
              Right(getOrAddScript(scriptPath))
            }
          }

          triedScript match {
            case Success(Right(script)) =>
              log.info(s"Adding script at path '$scriptPath' with ID '${script.id}' and URI '${script.uri}'")

              val erm = virtualMachine.eventRequestManager()
              val breakableLocations = locations.map(l => new BreakableLocation(breakpointIdGenerator.next, script, erm, l))
              addBreakableLocations(script, breakableLocations)

              Some(script)
            case Success(Left(msg)) =>
              log.info(s"Ignoring script because $msg")
              None
            case Failure(ex: FileNotFoundException) =>
              log.warn(s"Ignoring non-existent script at path '$scriptPath'")
              None
            case Failure(t) =>
              log.error(s"Ignoring script at path '$scriptPath'", t)
              None
          }
        case None =>
          log.info(s"Ignoring script type '${refType.name} because it has no line locations.")
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

  def initialize(): Done = {
    watchAddedClasses()
    log.debug("Postponing initialization until classes have stabilized")
    retryInitLater()
    Done
  }


  private def doInitialize(): Unit = {
    val referenceTypes = virtualMachine.allClasses()
    val typeCount = referenceTypes.size()

//    // Suspend VM while we're looking for types
//    log.info("Suspending virtual machine to do initialization")
//    virtualMachine.suspend()

    referenceTypes.asScala.foreach(considerReferenceType(_: ReferenceType, InitialScriptResolveAttempts))

    val breakableLocationCount = breakableLocationsByScriptUri.foldLeft(0)((sum, e) => sum + e._2.size)
    log.info(s"$typeCount types checked, ${scriptByPath.size} scripts added, $breakableLocationCount breakable locations identified")

    enableBreakingAtDebuggerStatement()

//    virtualMachine.resume()
//    log.info("Virtual machine resumed, listening for events...")

    isInitialized = true

    emitEvent(InitialInitializationComplete)

    Done
  }

  private def emitEvent(event: ScriptEvent): Unit = {
    // Emit asynchronously so that code that observes the event can interact with the host without deadlocking it.
    log.debug(s"Emitting event of type ${event.getClass.getSimpleName}")
    Future(eventSubject.onNext(event))
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

  def handleOperation(eventQueueItem: NashornScriptOperation): Done = eventQueueItem match {
    case NashornEventSet(eventSet) =>
      var doResume = true
      eventSet.asScala.foreach { ev =>
        try {
          ev match {
            case ev: BreakpointEvent =>

              attemptToResolveSourceLessReferenceTypes()

              // Disable breakpoints that were enabled once
              enabledBreakpoints.filter(_._2.isEnabledOnce).foreach { e =>
                e._2.disable()
                enabledBreakpoints -= e._1
              }

              doResume = handleBreakpoint(ev)

            case ev: ClassPrepareEvent =>
              if (isInitialized) {
                considerReferenceType(ev.referenceType(), InitialScriptResolveAttempts)
              } else {
                seenClassPrepareRequests += 1
              }
            case other =>
              log.debug("Unknown event: " + other)
          }
        } catch {
          case ex: Exception =>
            log.error(s"Failed to handle event ${ev.getClass.getName}", ex)
        }
      }
      if (doResume) eventSet.resume()
      Done
    case ConsiderReferenceType(refType, attemptsLeft) =>
      // We may have resolved the reference type when hitting a breakpoint, and in that case we can ignore this retry
      // attempt.
      if (scriptTypesToBreakRetryCycleFor.contains(refType)) {
        scriptTypesToBreakRetryCycleFor -= refType
      } else {
        considerReferenceType(refType, attemptsLeft)
      }
      Done
    case x@PostponeInitialize =>

      if (lastSeenClassPrepareRequests == seenClassPrepareRequests) doInitialize()
      else {
        lastSeenClassPrepareRequests = seenClassPrepareRequests
        retryInitLater()
      }
      Done
    case operation =>
      throw new IllegalArgumentException("Unknown operation: " + operation)
  }

  def functionDetails(functionMethod: Method): FunctionDetails = {
    FunctionDetails(functionMethod.name())
  }

  private def boxed(thread: ThreadReference, prim: PrimitiveValue, typeName: String, method: String): Value = {
    foundWantedTypes.get(typeName) match {
      case Some(aType) =>
        val invoker = new StaticInvoker(thread, aType)
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
  private def boxed(thread: ThreadReference, prim: PrimitiveValue): Value = prim match {
    case b: BooleanValue => boxed(thread, b, JL_Boolean, "valueOf(Z)Ljava/lang/Boolean;")
    case i: IntegerValue => boxed(thread, i, JL_Integer, "valueOf(I)Ljava/lang/Integer;")
    case l: LongValue =>
      // LongValue is kept for completeness - Nashorn since8 8u91 or something like that doesn't use Long for
      // representing numbers anymore.
      boxed(thread, l, JL_Long, "valueOf(J)Ljava/lang/Long;")
    case d: DoubleValue => boxed(thread, d, JL_Double, "valueOf(D)Ljava/lang/Double;")
    case _ => throw new IllegalArgumentException("Cannot box " + prim)
  }

  private def scopeWithFreeVariables(scopeObject: Value, freeVariables: Map[String, AnyRef])(implicit marshaller: Marshaller): Value = {
    require(scopeObject != null, "Scope object must be non-null")
    // If there aren't any free variables, we don't need to create a wrapper scope
    if (freeVariables.isEmpty) return scopeObject

    // Create a wrapper scope using ScriptRuntime.openWith, which corresponds to `with (obj) { ... }` in JS.
    foundWantedTypes.get(NIR_ScriptRuntime) match {
      case Some(scriptRuntime) =>
        // Just using "{}" returns undefined - don't know why - but "Object.create" works well.
        // Use the passed scope object as prototype object so that scope variables will be seen as well.
        val anObject = DebuggerSupport_eval(marshaller.thread, scopeObject, null, "Object.create(this)").asInstanceOf[ObjectReference]
        val mirror = new ScriptObjectMirror(anObject)
        freeVariables.foreach {
          case (name, value) =>
            val valueToPut = value match {
              case prim: PrimitiveValue => boxed(marshaller.thread, prim)
              case other => other
            }
            mirror.put(name, valueToPut, isStrict = false)
        }

        anObject

      case None =>
        log.warn("Don't have the ScriptRuntime type available to wrap the scope.")
        scopeObject
    }
  }

  private def DebuggerSupport_eval(thread: ThreadReference, thisObject: Value, scopeObject: Value, code: String): Value  = {
    foundWantedTypes.get(NIR_DebuggerSupport) match {
      case Some(ct: ClassType) =>
        val invoker = new StaticInvoker(thread, ct)

        // eval(ScriptObject scope, Object self, String string, boolean returnException
        invoker.eval(scopeObject, thisObject, code, true)

      case _ =>
        throw new IllegalStateException("The DebuggerSupport type wasn't found, cannot evaluate code.")
    }
  }

  // Determines if the location is in ScriptRuntime.DEBUGGER.
  private def isDebuggerStatementLocation(loc: Location)=
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

  private def prototypeOf(marshaller: Marshaller, value: Value): Option[Value] = value match {
    case ref: ObjectReference =>
      val invoker = new DynamicInvoker(marshaller.thread, ref)
      val maybeProto = Option(invoker.getProto())
      // Prototype of Global is jdk.nashorn.internal.objects.NativeObject$Prototype - ignore it
      if (maybeProto.exists(_.`type`().name().endsWith("$Prototype"))) None
      else maybeProto
    case _ => None
  }

  private def prototypeChain(marshaller: Marshaller, value: Value): Seq[Value] = prototypeOf(marshaller, value) match {
    case Some(proto) => Seq(proto) ++ prototypeChain(marshaller, proto)
    case None => Seq.empty
  }

  private def createScopeChain(marshaller: Marshaller, originalScopeValue: Option[Value], thisValue: Value, marshalledThisNode: ValueNode, localNode: Option[ObjectNode]): Seq[Scope] = {
    // Note: I tried to mimic how Chrome reports scopes, but it's a bit difficult. For example, if the current scope
    // is a 'with' scope, there is no way (that I know of) to determine if we're in a function (IIFE) inside a with
    // block or if we're inside a with block inside a function.
    def toScope(v: Value) = Scope(marshaller.marshal(v), scopeTypeFromValueType(v))
    def findGlobalScope(): Option[Scope] = {
      if (scopeTypeFromValueType(thisValue) == ScopeType.Global) {
        // this == global, so no need to call DebuggerSupport.getGlobal().
        Some(Scope(marshalledThisNode, ScopeType.Global))
      } else {
        foundWantedTypes.get(NIR_DebuggerSupport) match {
          case Some(debuggerSupport) =>
            val invoker = new StaticInvoker(marshaller.thread, debuggerSupport)
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
        scopeChain ++= prototypeChain(marshaller, v).map(toScope)
      case None =>
        // noop
    }

    // Make sure we have a global scope lsat!
    if (!scopeChain.exists(_.scopeType == ScopeType.Global)) {
      scopeChain ++= findGlobalScope()
    }

    scopeChain
  }

  private def buildStackFramesSequence(perStackFrame: Seq[(Map[String, Value], Location)], thread: ThreadReference): Seq[StackFrameHolder] = {
    implicit val marshaller = new Marshaller(thread, mappingRegistry)
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

            // Create an artificial object node to hold the locals. Note that the object ID must be unique per stack
            // since we store object nodes in a map.
            val locals = localValues.map(e => e._1 -> marshaller.marshal(e._2))
            val localNode = if (locals.nonEmpty) {
              val node = ObjectNode(ObjectId("$$locals-" + stackframeId))
              mappingRegistry.register(null, node, locals)
              Some(node)
            } else None

            val scopeChain = createScopeChain(marshaller, originalScope, originalThis, thisObj, localNode)

            def evaluateCodeOnFrame: CodeEvaluator = {
              case (code, namedValues) =>
                // If we don't have :scope, use :this - it's used as a parent object for the created 'with' object.
                val scopeToUse = scopeWithFreeVariables(originalScope.getOrElse(originalThis), namedValues ++ localValues)

                try {
                  val ret = DebuggerSupport_eval(thread, originalThis, scopeToUse, code)
                  marshaller.marshal(ret)
                } catch {
                  case ex: Exception =>
                    log.error("Code evaluation failed.", ex)
                    throw ex
                }
            }

            try {
              findBreakableLocation(location).map(w => new StackFrameImpl(stackframeId, thisObj, scopeChain, w, evaluateCodeOnFrame, functionDetails(functionMethod))) match {
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
          case None if isDebuggerStatementLocation(location) =>
            StackFrameHolder(None, isAtDebuggerStatement = true)
          case _ =>
            StackFrameHolder(None)
        }

    }
  }

  private def handleBreakpoint(ev: LocatableEvent): Boolean = {
    // Resume right away if we're not pausing on breakpoints
    if (!willPauseOnBreakpoints) return true

    log.info(s"A breakpoint was hit at location ${ev.location()} in thread ${ev.thread().name()}")
    val thread = ev.thread()

    // Start with a fresh object registry
    objectPairById.clear()

    // Get all Values FIRST, before marshalling. This is because marshalling requires us to call methods, which
    // will temporarily resume threads, which causes the stack frames to become invalid.
    val perStackFrame = thread.frames().asScala.map { sf =>
      // In the step tests, I get JDWP error 35 (INVALID SLOT) for ':return' and since we don't use it, leave it for
      // now. If we need it, we can get it separately.
      val variables = Try(sf.visibleVariables()).getOrElse(Collections.emptyList()).asScala.filter(_.name() != ":return").asJava
      val values = sf.getValues(variables).asScala.map(e => e._1.name() -> e._2)
      (values.toMap, sf.location())
    }

    // Second pass, marshal
    val stackFrames = buildStackFramesSequence(perStackFrame, thread)

    stackFrames.headOption match {
      case Some(holder) if holder.stackFrame.isEmpty && !holder.isAtDebuggerStatement =>
        // First/top stack frame doesn't belong to a script. Resume!
        log.debug(s"Ignoring breakpoint at ${ev.location()} because it doesn't belong to a script.")
        true
      case Some(holder) =>
        if (holder.isAtDebuggerStatement) log.debug("Breakpoint is at JavaScript 'debugger' statement")
        doPause(thread, stackFrames.flatMap(_.stackFrame))
        // Resume will be controlled externally
        false
      case None =>
        // Hm, no stack frames at all... Resume!
        log.debug(s"Ignoring breakpoint at ${ev.location()} because no stack frames were found at all.")
        true
    }
  }

  private def doPause(thread: ThreadReference, stackFrames: Seq[StackFrame]): Unit = {
    pausedData = Some(new PausedData(thread, stackFrames))

    val breakpoint = stackFrames.head.breakpoint
    scriptById(breakpoint.scriptId).foreach {
      case s: ScriptImpl =>
        val line = s.lines(breakpoint.lineNumberBase1 - 1)
        log.debug(s"Pausing at ${s.uri}:${breakpoint.lineNumberBase1}: $line")
      case _ =>
    }

    val hitBreakpoint = HitBreakpoint(stackFrames)
    emitEvent(hitBreakpoint)
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
      return "file:/" + typeName.replace('.', '/').replace('\\', '/').replaceAll("[$^]", "_")
    }
    if (path.startsWith("file:/")) new File(new URI(path)).getAbsolutePath else path
  }


  private def getOrAddScript(path: String): Script =
    scriptByPath.getOrElseUpdate(path, ScriptImpl.fromFile(path, scriptIdGenerator.next))

  private def getOrAddEvalScript(artificialPath: String, source: String): Script =
    scriptByPath.getOrElseUpdate(artificialPath, ScriptImpl.fromSource(artificialPath, source, scriptIdGenerator.next))

  override def scripts: Seq[Script] = scriptByPath.values.toSeq

  override def scriptById(id: String): Option[Script] = scripts.find(_.id == id) //TODO: make more efficient

  override def events: Observable[ScriptEvent] = eventSubject

  override def setBreakpoint(scriptUri: String, lineNumberBase1: Int): Breakpoint = {
    findBreakableLocation(scriptUri, lineNumberBase1) match {
      case Some(br) =>
        if (br.lineNumber != lineNumberBase1) {
          log.info(s"Client asked for a breakpoint at line $lineNumberBase1 in $scriptUri, setting it at line ${br.lineNumber}.")
        } else {
          log.info(s"Setting a breakpoint at line ${br.lineNumber} in $scriptUri")
        }

        br.enable()
        enabledBreakpoints += (br.id -> br)
        br.toBreakpoint

      case None =>
        throw new IllegalArgumentException(s"Cannot identify location '$scriptUri', line $lineNumberBase1")
    }
  }

  private def findBreakableLocation(location: Location): Option[BreakableLocation] = {
    scriptByPath.get(scriptPathFromLocation(location)).flatMap(s => findBreakableLocation(s.uri, location.lineNumber()))
  }

  private def findBreakableLocation(scriptUri: String, lineNumber: Int): Option[BreakableLocation] = {
    breakableLocationsByScriptUri.get(scriptUri).flatMap { breakableLocations =>
      // TODO: Is it good to filter with >= ? The idea is to create a breakpoint even if the user clicks on a line that
      // TODO: isn't "breakable".
      val candidates = breakableLocations.filter(_.lineNumber >= lineNumber).sortWith((b1, b2) => b1.lineNumber < b2.lineNumber)
      candidates.headOption
    }
  }

  private def resumeWhenPaused(): Unit = pausedData match {
    case Some(data) =>
      log.info("Resuming virtual machine")
      virtualMachine.resume()
      pausedData = None
      objectPairById.clear() // only valid when paused
      emitEvent(Resumed)
    case None =>
      log.debug("Ignoring resume request when not paused (no pause data).")
  }

  override def resume(): Done = {
    resumeWhenPaused()
    Done
  }

  private def removeAllBreakpoints(): Done = {
    enabledBreakpoints.foreach(e => e._2.disable())
    enabledBreakpoints.clear()
    Done
  }

  override def reset(): Done = {
    log.info("Resetting VM...")
    willPauseOnBreakpoints = false
    removeAllBreakpoints()
    resume()
  }

  override def removeBreakpointById(id: String): Done = {
    enabledBreakpoints.get(id) match {
      case Some(bp) =>
        log.info(s"Removing breakpoint with id $id")
        bp.disable()
        enabledBreakpoints -= bp.id
      case None =>
        log.warn(s"Got request to remove an unknown breakpoint with id $id")
    }
    Done
  }

  private def enableBreakpointOnce(bl: BreakableLocation): Unit = {
    bl.enableOnce()
    enabledBreakpoints += (bl.id -> bl)
  }

  private def expensiveStepInto(): Unit = {
    // Creating a step request with STEP_INTO didn't work well in my testing, since the VM seems to end up in some
    // sort of call site method. Therefore we do this one a bit differently.
    log.debug("Performing expensive step-into by one-off-enabling all breakpoints.")
    // Do a one-off enabling of non-enabled breakpoints
    breakableLocationsByScriptUri.flatMap(_._2).withFilter(!_.isEnabled).foreach(enableBreakpointOnce)
  }

  private def setTemporaryBreakpointsInStackFrame(stackFrame: StackFrame): Int = stackFrame match {
    case sf: StackFrameImpl =>
      // Set one-off breakpoints in all locations of the method of this stack frame
      val scriptUri = sf.breakableLocation.script.uri
      val allBreakableLocations = breakableLocationsByScriptUri(scriptUri)
      val sfMethod = sf.breakableLocation.location.method()
      val sfLineNumber = sf.breakableLocation.location.lineNumber()
      val relevantBreakableLocations = allBreakableLocations.filter(bl => bl.location.method() == sfMethod && bl.location.lineNumber() > sfLineNumber)
      relevantBreakableLocations.foreach(enableBreakpointOnce)
      relevantBreakableLocations.size
    case other =>
      log.warn("Unknown stack frame type: " + other)
      0
  }

  private def stepOut(pd: PausedData): Unit = {
    // For step out, we set breakpoints in the parent stackframe (head of the tail)
    pd.stackFrames.tail.headOption match {
      case Some(sf) =>
        val breakpointCount = setTemporaryBreakpointsInStackFrame(sf)
        if (breakpointCount > 0) {
          log.debug(s"Performing step-out by one-off-enabling $breakpointCount breakpoints in the parent stack frame.")
        } else {
          log.warn("Turning step-out request to normal resume since no breakable locations were found in the parent script frame.")
        }

      case None =>
        log.info("Turning step-out request to normal resume since there is no parent script stack frame.")
        // resumeWhenPaused is called by caller method (step)
    }
  }

  private def stepOver(pd: PausedData): Unit = {
    // For step over, we set breakpoints in the top stackframe _and_ the parent stack frame
    val breakpointCount = pd.stackFrames.take(2).map(setTemporaryBreakpointsInStackFrame).sum
    if (breakpointCount > 0) {
      log.debug(s"Performing step-over by one-off-enabling $breakpointCount breakpoints in the current and parent stack frames.")
    } else {
      log.warn("Turning step-over request to normal resume since no breakable locations were found in the current and parent script frames.")
    }
  }

  override def step(stepType: StepType): Done = pausedData match {
    case Some(pd) =>
      log.info(s"Stepping with type $stepType")
      // Note that we don't issue normal step requests to the remove VM, because a script line != a Java line, so if we
      // were to request step out, for example, we might end up in some method that acts as a script bridge.
      stepType match {
        case StepInto =>
          expensiveStepInto()
        case StepOver =>
          stepOver(pd)
        case StepOut =>
          stepOut(pd)
      }

      resumeWhenPaused()
      Done
    case None =>
      throw new IllegalStateException("A breakpoint must be active for stepping to work")
  }

  override def evaluateOnStackFrame(stackFrameId: String, expression: String, namedObjects: Map[String, ObjectId]): Try[ValueNode] = Try {
    pausedData match {
      case Some(pd) =>
        findStackFrame(pd, stackFrameId) match {
          case Some(sf: StackFrameImpl) =>

            // Get the Value instances corresponding to the named objects
            val namedValues = namedObjects.map {
              case (name, objectId) =>
                objectPairById.get(objectId) match {
                  // TODO: Should we handle extras here?
                  case Some((maybeValue, _, _)) if maybeValue.isDefined => name -> maybeValue.get
                  case _ => throw new IllegalArgumentException(s"No object with ID '$objectId' was found.")
                }
            }

            sf.eval(expression, namedValues)
          case _ =>
            log.warn(s"No stack frame found with ID $stackFrameId. Available IDs: " + pd.stackFrames.map(_.id).mkString(", "))
            throw new IllegalArgumentException(s"Failed to find a stack frame with ID $stackFrameId")
        }
      case None =>
        throw new IllegalStateException("Code evaluation can only be done in a paused state.")
    }
  }

  private def findStackFrame(pausedData: PausedData, id: String): Option[StackFrame] = {
    if (id == "$top") return pausedData.stackFrames.headOption
    pausedData.stackFrames.find(_.id == id)
  }

  override def pauseOnBreakpoints(): Done = {
    log.info("Will pause on breakpoints")
    willPauseOnBreakpoints = true
    Done
  }

  override def ignoreBreakpoints(): Done = {
    log.info("Will ignore breakpoints")
    willPauseOnBreakpoints = false
    Done
  }

  private def propertiesFromJSObject(jsObject: ObjectReference, isArray: Boolean)(implicit marshaller: Marshaller): Map[String, ObjectPropertyDescriptor] = {
    import com.programmaticallyspeaking.ncd.infra.StringUtils._
    val mirror = new JSObjectMirror(jsObject)

    // For an array, keySet should return indices + "length", and then we get use getMember. An alternative would be to
    // use getSlot, but that'd require us to figure out the length (or just increase until hasSlot returns false).
    val properties = mirror.keySet()

    properties.map { prop =>
      val theValue = mirror.getUnknown(prop) match {
        case EmptyNode|SimpleValue(Undefined) if isArray && isUnsignedInt(prop) =>
          // For a slot-based array JSObject, getMember may return nothing (which admittedly is wrong).
          mirror.getSlot(prop.toInt)
        case other => other
      }

      prop -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, true, true, false,
        Option(theValue), None, None)
    }.toMap
  }


  private def propertiesFromScriptObject(scriptObject: ObjectReference, onlyOwn: Boolean, onlyAccessors: Boolean)(implicit marshaller: Marshaller): Map[String, ObjectPropertyDescriptor] = {
    val thread = marshaller.thread
    val mirror = new ScriptObjectMirror(scriptObject)

    val propertyNames = if (onlyOwn) {
      // Get own properties, pass true to get non-enumerable ones as well (because they are relevant for debugging)
      mirror.getOwnKeys(true)
    } else {
      // Get all properties - this method walks the prototype chain
      mirror.propertyIterator().toArray
    }
    propertyNames.map { prop =>
      // Get either only the own descriptor or try both ways (own + proto). This is required for us to know
      // if the descriptor represents an own property.
      val ownDescriptor = mirror.getOwnPropertyDescriptor(prop)
      // TODO ugly, ugly.. make nicer
      val hasOwnDescriptor = ownDescriptor.isDefined

      val protoDescriptor = if (onlyOwn || hasOwnDescriptor) None else mirror.getPropertyDescriptor(prop)

      val descriptorToUse = protoDescriptor.orElse(ownDescriptor)
        .getOrElse(throw new IllegalStateException(s"No property descriptor for ${scriptObject.`type`().name()}.$prop"))

      // Read descriptor-generic information
      val theType = descriptorToUse.getType
      val isConfigurable = descriptorToUse.isConfigurable
      val isEnumerable = descriptorToUse.isEnumerable
      val isWritable = descriptorToUse.isWritable

      prop -> (theType match {
        case 0 =>
          // Generic
          ObjectPropertyDescriptor(PropertyDescriptorType.Generic, isConfigurable, isEnumerable, isWritable, hasOwnDescriptor,
            None, None, None)
        case 1 =>
          // Data, value is ok to use
          val theValue = descriptorToUse.getValue
          ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable, isEnumerable, isWritable, hasOwnDescriptor,
            Option(theValue), None, None)
        case 2 =>
          // Accessor, getter/setter are ok to use
          val getter = descriptorToUse.getGetter
          val setter = descriptorToUse.getSetter
          ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, isConfigurable, isEnumerable, isWritable, hasOwnDescriptor,
            None, Option(getter), Option(setter))
        case other => throw new IllegalArgumentException("Unknown property descriptor type: " + other)
      })
    }.filter(e => !onlyAccessors || e._2.descriptorType == PropertyDescriptorType.Accessor).toMap
  }

  override def getObjectProperties(objectId: ObjectId, onlyOwn: Boolean, onlyAccessors: Boolean): Map[String, ObjectPropertyDescriptor] = pausedData match {
    case Some(pd) =>
      implicit val marshaller = new Marshaller(pd.thread, mappingRegistry)
      objectPairById.get(objectId) match {
        case Some((maybeValue, node, extraEntries)) =>
          // If we have a script object, get properties from it
          val scriptObjectProps = maybeValue match {
            case Some(ref: ObjectReference) if marshaller.isScriptObject(ref) =>
              propertiesFromScriptObject(ref, onlyOwn, onlyAccessors)
            case Some(ref: ObjectReference) if marshaller.isJSObject(ref) =>
              propertiesFromJSObject(ref, node.isInstanceOf[ArrayNode])
            case _ => Map.empty
          }

          // In addition, the node may contain extra entries that typically do not come from Nashorn. One example is
          // the Java stack we add if we detect a Java exception.
          val extraProps = extraEntries.map(e => {
            e._1 -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true, isWritable = false,
              isOwn = true, Some(e._2), None, None)
          })

          // Combine the two maps
          scriptObjectProps ++ extraProps

        case None =>
          log.warn (s"Unknown object ($objectId), cannot get properties")
          Map.empty
      }
    case None =>
      throw new IllegalStateException("Property extraction can only be done in a paused state.")
  }

  class PausedData(val thread: ThreadReference, val stackFrames: Seq[StackFrame])

//  class StackFrameImpl(val thisObj: ValueNode, val scopeObj: Option[ValueNode], val locals: ObjectNode,
  class StackFrameImpl(val id: String, val thisObj: ValueNode, val scopeChain: Seq[Scope],
                       val breakableLocation: BreakableLocation,
                       val eval: CodeEvaluator,
                       val functionDetails: FunctionDetails) extends StackFrame {
    val breakpoint = breakableLocation.toBreakpoint
  }

  case class StackFrameHolder(stackFrame: Option[StackFrame], isAtDebuggerStatement: Boolean = false)
}

object InitialInitializationComplete extends ScriptEvent