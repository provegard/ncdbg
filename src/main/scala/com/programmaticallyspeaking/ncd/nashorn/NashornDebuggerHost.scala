package com.programmaticallyspeaking.ncd.nashorn

import java.io.{File, FileNotFoundException}
import java.net.URI
import java.util.Collections

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.programmaticallyspeaking.ncd.messaging.{Observable, Subject}
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi.event._
import com.sun.jdi.request.{BreakpointRequest, EventRequest, EventRequestManager, StepRequest}
import com.sun.jdi.{StackFrame => _, _}
import org.slf4s.Logging

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object NashornDebuggerHost {
  val NIR_DebuggerSupport = "jdk.nashorn.internal.runtime.DebuggerSupport"
  val NIR_ScriptRuntime = "jdk.nashorn.internal.runtime.ScriptRuntime"

  // The name of the DEBUGGER method in the ScriptRuntime class
  val ScriptRuntime_DEBUGGER = "DEBUGGER"

  val wantedTypes = Set(
    NIR_DebuggerSupport,
    NIR_ScriptRuntime
  )

  type CodeEvaluator = (String, Map[String, AnyRef]) => ValueNode

  def optionToEither[A](opt: Option[A], msg: => String): Either[String, A] = opt match {
    case Some(a) => Right(a)
    case None => Left(msg)
  }
}

class NashornDebuggerHost(val virtualMachine: VirtualMachine) extends ScriptHost with Logging {
  import scala.collection.JavaConversions._
  import NashornDebuggerHost._

  private val scriptByPath = mutable.Map[String, Script]()

  private val breakableLocationsByScriptUri = mutable.Map[String, ListBuffer[BreakableLocation]]()
  private val enabledBreakpoints = mutable.Map[String, BreakableLocation]()

  private val scriptIdGenerator = new IdGenerator("nds")
  private val breakpointIdGenerator = new IdGenerator("ndb")
  private val stackframeIdGenerator = new IdGenerator("ndsf")

  private val eventSubject = Subject.serialized[ScriptEvent]

  // Since we effectively hand out this map via the `objectRegistry` method, it needs to be thread safe.
  private val objectPairById = TrieMap[ObjectId, (Option[Value], ComplexNode)]()

  private lazy val objectReg = new ObjectRegistry {
    override def objectById(id: ObjectId): Option[ComplexNode] = objectPairById.get(id).map(_._2)
  }

  private val foundWantedTypes = mutable.Map[String, ClassType]()

  // Data that are defined when the VM has paused on a breakpoint or encountered a step event
  private var pausedData: Option[PausedData] = None

  /**
    * By default, we don't pause when a breakpoint is hit. This is important since we add a fixed breakpoint for
    * JS 'debugger' statements, and we don't want that to pause the VM when a debugger hasn't attached yet.
    */
  private var willPauseOnBreakpoints = false

  private def addBreakableLocations(script: Script, breakableLocations: Seq[BreakableLocation]): Unit = {
    breakableLocationsByScriptUri.getOrElseUpdate(script.uri, ListBuffer.empty) ++= breakableLocations
  }

  private def enableBreakingAtDebuggerStatement(erm: EventRequestManager): Unit = {
    val debuggerLoc = for {
      scriptRuntime <- optionToEither(foundWantedTypes.get(NIR_ScriptRuntime), "no ScriptRuntime type found")
      debuggerMethod <- optionToEither(scriptRuntime.methodsByName(ScriptRuntime_DEBUGGER).headOption, "ScriptRuntime.DEBUGGER method not found")
      location <- optionToEither(debuggerMethod.allLineLocations().headOption, "no line location found in ScriptRuntime.DEBUGGER")
    } yield location

    debuggerLoc match {
      case Right(location) =>
        log.info("Enabling automatic breaking at JavaScript 'debugger' statements")
        // TODO: BreakableLocation also does this. Reuse code!
        val br = erm.createBreakpointRequest(location)
        br.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
        br.setEnabled(true)
      case Left(msg) =>
        log.warn(s"Won't be able to break at JavaScript 'debugger' statements because $msg")
    }
  }

  def startListening(): Unit = {
    val erm = virtualMachine.eventRequestManager()
    val referenceTypes = virtualMachine.allClasses()
    val typeCount = referenceTypes.size()
    var scriptCount = 0

    referenceTypes.foreach { refType =>
      val className = refType.name()

      if (wantedTypes.contains(className)) {
        refType match {
          case ct: ClassType =>
            log.debug(s"Found the $className type")
            foundWantedTypes += className -> ct
          case other =>
            log.warn(s"Found the $className type but it's a ${other.getClass.getName} rather than a ClassType")
        }
      } else if (className.startsWith("jdk.nashorn.internal.scripts.Script$")) {
        // This is a compiled Nashorn script class.
        log.debug(s"Script reference type: ${refType.name}")
        scriptCount += 1

        val locations = refType.allLineLocations().toSeq
        locations.headOption match {
          case Some(firstLocation) =>
            val scriptPath = scriptPathFromLocation(firstLocation)
            log.info(s"Adding script at path: $scriptPath")

            // Create and add the Script object. Note that we may hit the same script multiple times, e.g. if the
            // script contains inner functions, which are compiled into separate classes.
            getOrAddScript(scriptPath) match {
              case Success(script) =>

              val breakableLocations = locations.map(l => new BreakableLocation(breakpointIdGenerator.next, script, erm, l))
              addBreakableLocations(script, breakableLocations)

              case Failure(ex: FileNotFoundException) =>
                log.warn(s"Ignoring non-existent script at path '$scriptPath'")
              case Failure(t) =>
                log.error(s"Ignoring script at path '$scriptPath'", t)
            }
          case None =>
            log.info(s"Ignoring script type '${refType.name} because it has no line locations.")
        }
      }
    }

    val breakableLocationCount = breakableLocationsByScriptUri.foldLeft(0)((sum, e) => sum + e._2.size)
    log.info(s"$typeCount types checked, $scriptCount scripts detected, ${scriptByPath.size} scripts added, $breakableLocationCount breakable locations identified")

    enableBreakingAtDebuggerStatement(erm)

    //TODO: Is resume needed?
//    vm.resume()
//    log.info("Virtual Machine resumed, listening for events...")

    try {
      listenForEvents()
    } catch {
      case ex: VMDisconnectedException =>
        log.info("The remote VM disconnected!")
        eventSubject.onComplete()
        throw ex
      case ex: Exception =>
        log.error("An unknown error occurred.", ex)
        eventSubject.onComplete()
        throw ex
    }
  }

  private def listenForEvents(): Unit = {
    listenIndefinitely(virtualMachine.eventQueue())
  }

  private def handleEventSet(eventSet: EventSet): Unit = {
    var doResume = true
    eventSet.foreach { ev =>
      try {
        ev match {
          case ev: BreakpointEvent =>

            // Disable breakpoints that were enabled once
            enabledBreakpoints.filter(_._2.isEnabledOnce).foreach { e =>
              e._2.disable()
              enabledBreakpoints -= e._1
            }

            doResume = handleBreakpoint(ev)
          case ev: StepEvent =>
            // Only one step event per thread is allowed, so delete this one right away
            ev.virtualMachine().eventRequestManager().deleteEventRequest(ev.request())

            doResume = handleBreakpoint(ev)
          case other =>
            log.debug("Unknown event: " + other)
        }
      } catch {
        case ex: Exception =>
          log.error(s"Failed to handle event $ev", ex)
      }
    }
    if (doResume) eventSet.resume()
  }

  def functionDetails(functionMethod: Method): FunctionDetails = {
    FunctionDetails(functionMethod.name())
  }

  private def scopeWithFreeVariables(thread: ThreadReference, scopeObject: Value, freeVariables: Map[String, AnyRef]): Value = {
    // If there aren't any free variables, we don't need to create a wrapper scope
    if (freeVariables.isEmpty) return scopeObject

    // Create a wrapper scope using ScriptRuntime.openWith, which corresponds to `with (obj) { ... }` in JS.
    foundWantedTypes.get(NIR_ScriptRuntime) match {
      case Some(scriptRuntime) =>
        // Just using "{}" returns undefined - don't know why - but "Object.create" works well.
        val anObject = DebuggerSupport_eval(thread, null, null, "Object.create(null)").asInstanceOf[ObjectReference]
        val mirror = new ScriptObjectMirror(thread, anObject)
        freeVariables.foreach {
          case (name, value) =>
            mirror.put(name, value, isStrict = false)
        }

        new StaticInvoker(thread, scriptRuntime).openWith(scopeObject, anObject)

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

  private def buildStackFramesSequence(perStackFrame: Seq[(Map[String, Value], Location)], thread: ThreadReference, mappingRegistry: MappingRegistry): Seq[StackFrameHolder] = {
    val marshaller = new Marshaller(thread, mappingRegistry)
    perStackFrame.map {
      case (values, location) =>
        val marshalled = values.map(e => e._1 -> marshaller.marshal(e._2))
        val functionMethod = location.method()

        // ":this" should always be present, but a function that doesn't capture anything may lack a ":scope" variable
        marshalled.get(":this") match {
          case Some(thisObj) =>
            val scopeObj = marshalled.get(":scope").orNull

            def evaluateCodeOnFrame: CodeEvaluator = {
              case (code, namedValues) =>
                val originalThis = values(":this")
                val originalScope = scopeWithFreeVariables(thread, values.get(":scope").orNull, namedValues)

                try {
                  val ret = DebuggerSupport_eval(thread, originalThis, originalScope, code)
                  marshaller.marshal(ret)
                } catch {
                  case ex: Exception =>
                    log.error("Code evaluation failed.", ex)
                    throw ex
                }
            }

            // Variables that don't start with ":" are locals
            val locals = marshalled.filter(e => !e._1.startsWith(":")).toMap

            // Create an artificial object node to hold the locals
            val localNode = ObjectNode(locals.map(e => e._1 -> LazyNode.eager(e._2)), ObjectId("$$locals"))
            mappingRegistry.register(null, localNode)

            try {
              findBreakableLocation(location).map(w => new StackFrameImpl(thisObj, Option(scopeObj), localNode, w.toBreakpoint, evaluateCodeOnFrame, functionDetails(functionMethod))) match {
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
          case _ => StackFrameHolder(None)
        }

    }
  }

  private def handleBreakpoint(ev: LocatableEvent): Boolean = {
    // Resume right away if we're not pausing on breakpoints
    if (!willPauseOnBreakpoints) return true

    log.debug(s"A breakpoint was hit at location ${ev.location()} in thread ${ev.thread().name()}")
    val thread = ev.thread()

    // Start with a fresh object registry
    objectPairById.clear()

    // Get all Values FIRST, before marshalling. This is because marshalling requires us to call methods, which
    // will temporarily resume threads, which causes the stack frames to become invalid.
    val perStackFrame = thread.frames().map { sf =>
      val variables = Try(sf.visibleVariables()).getOrElse(Collections.emptyList())
      val values = sf.getValues(variables).map(e => e._1.name() -> e._2)
      (values.toMap, sf.location())
    }

    // Second pass, marshal
    val mappingRegistry = new MappingRegistry {
      override def register(value: Value, valueNode: ValueNode): Unit = valueNode match {
        case c: ComplexNode =>
          objectPairById += c.objectId -> (Option(value), c)
        case _ => // ignore
      }
    }
    val stackFrames = buildStackFramesSequence(perStackFrame, thread, mappingRegistry)

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

    val hitBreakpoint = HitBreakpoint(stackFrames)
    eventSubject.onNext(hitBreakpoint)
  }

  @tailrec
  private def listenIndefinitely(queue: EventQueue): Unit = {
    Option(queue.remove(1000)).foreach(handleEventSet)
    listenIndefinitely(queue)
  }

  private def scriptPathFromLocation(location: Location): String = {
    // It appears *name* is a path on the form 'file:/c:/...', whereas path has a namespace prefix
    // (jdk\nashorn\internal\scripts\). This seems to be consistent with the documentation (although it's a bit
    // surprising), where it is stated that the Java stratum doesn't use source paths and a path therefore is a
    // package-qualified file name in path form, whereas name is the unqualified file name (e.g.:
    // java\lang\Thread.java vs Thread.java).
    val path = location.sourceName()
    if (path.startsWith("file:/")) new File(new URI(path)).getAbsolutePath else path
  }


  private def getOrAddScript(path: String): Try[Script] =
    Try(scriptByPath.getOrElseUpdate(path, ScriptImpl.fromFile(path, scriptIdGenerator.next)))

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

  override def objectRegistry: ObjectRegistry = objectReg

  private def resumeWhenPaused(): Unit = pausedData match {
    case Some(data) =>
      log.info("Resuming virtual machine")
      virtualMachine.resume()
      pausedData = None
      objectPairById.clear() // only valid when paused
      eventSubject.onNext(Resumed)
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
      case Some(bp) =>
        log.info(s"Removing breakpoint with id $id")
        bp.disable()
        enabledBreakpoints -= bp.id
      case None =>
        log.warn(s"Got request to remove an unknown breakpoint with id $id")
    }
  }

  private def expensiveStepInto(): Unit = {
    // Do a one-off enabling of non-enabled breakpoints
    breakableLocationsByScriptUri.flatMap(_._2).withFilter(!_.isEnabled).foreach { bl =>
      bl.enableOnce()
      enabledBreakpoints += (bl.id -> bl)
    }
  }

  override def step(stepType: StepType): Unit = pausedData match {
    case Some(pd) =>
      log.info(s"Stepping with type $stepType")
      val depth = stepType match {
        case StepInto => StepRequest.STEP_INTO
        case StepOver => StepRequest.STEP_OVER
        case StepOut => StepRequest.STEP_OUT
      }

      // TODO: Step-out doesn't work because we'll end up in java.lang.invoke.LambdaForm$DMH.653687670.invokeStatic_LL_
      // TODO: So we need to do something similar to Step-into.

      if (depth == StepRequest.STEP_INTO) {
        // Creating a step request with STEP_INTO didn't work well in my testing, since the VM seems to end up in some
        // sort of call site method. Therefore we do this one a bit differently.
        log.debug("Performing expensive step-into by one-off-enabling all breakpoints.")
        expensiveStepInto()
        resumeWhenPaused()
      } else {
        val req = virtualMachine.eventRequestManager().createStepRequest(pd.thread, StepRequest.STEP_LINE, depth)
        req.addCountFilter(1) // next step only
        req.enable()
        resumeWhenPaused()
      }
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
                  case Some((maybeValue, _)) if maybeValue.isDefined => name -> maybeValue.get
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

  class PausedData(val thread: ThreadReference, val stackFrames: Seq[StackFrame])

  class StackFrameImpl(val thisObj: ValueNode, val scopeObj: Option[ValueNode], val locals: ObjectNode, val breakpoint: Breakpoint,
                       val eval: CodeEvaluator,
                       val functionDetails: FunctionDetails) extends StackFrame {
    // Generate an ID for the stack frame so that we can find it later when asked to evaluate code for a
    // particular stack frame.
    val id = stackframeIdGenerator.next
  }

  case class StackFrameHolder(stackFrame: Option[StackFrame], isAtDebuggerStatement: Boolean = false)

  override def pauseOnBreakpoints(): Unit = {
    log.info("Will pause on breakpoints")
    willPauseOnBreakpoints = true
  }

  override def ignoreBreakpoints(): Unit = {
    log.info("Will ignore breakpoints")
    willPauseOnBreakpoints = false
  }
}