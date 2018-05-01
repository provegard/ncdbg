package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.infra.{CancellableFuture, DelayedFuture}
import com.programmaticallyspeaking.ncd.messaging.{Observer, SerializedSubject}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{ScriptClassNamePrefix, wantedTypes}
import com.sun.jdi.event.ClassPrepareEvent
import com.sun.jdi.request.EventRequest
import com.sun.jdi._
import org.slf4s.Logging

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

sealed trait ScanAction
case class ScanMoreLater(continueScan: () => Unit) extends ScanAction

object ClassScanner {
  /**
    * Scanning all classes in one go can be problematic since it may take a long time, thereby blocking host
    * operations. This constant defines the time we spend scanning classes before we yield to other operations.
    */
  val SCAN_CLASSES_BATCH_LEN = 2.seconds
}

class ClassScanner(virtualMachine: XVirtualMachine, scripts: Scripts, scriptFactory: ScriptFactory,
                   scriptPublisher: ScriptPublisher, breakableLocations: BreakableLocations,
                   activeBreakpoints: ActiveBreakpoints,
                   actionPerWantedType: Map[String, (ClassType) => Unit])(implicit executionContext: ExecutionContext) extends Logging {
  import ClassScanner._

  import scala.collection.JavaConverters._

  private var hasInitiatedClassScanning = false

  private val foundWantedTypes = TrieMap[String, ClassType]()

  private val subject = new SerializedSubject[ScanAction]()

  private var scanTimer: Option[CancellableFuture[Unit]] = None

  /**
    * Populated/updated during a class scan to track the classes left to scan.
    */
  private var classesToScan = List.empty[ReferenceType]
  private var totalScanCount: Int = 0

  private var actionsOnComplete = List.empty[() => Unit]

  def handleEvent(ev: ClassPrepareEvent): Unit = consider(ev.referenceType(), Some(ev.thread()))

  /**
    * Execute an operation and then scan added classes to see if there are any new scripts.
    *
    * @param f the operation
    * @tparam R the operation return type
    * @return the operation return value
    */
  def withClassTracking[R](f: => R): R = {
    // We evaluate code with all ClassPrepare requests disabled to avoid deadlock. However, code evaluation may
    // result in an added script (e.g. when the 'load' extension is used). Track added classes and let the
    // scanner decide if scripts were added.
    val classTracker = new ClassTracker(virtualMachine.inner)

    var thrown: Throwable = null
    try f catch {
      case t: Throwable =>
        thrown = t
        throw t
    } finally {
      val addedClasses = classTracker.addedClasses()
      try {
        log.debug(s"Scanning ${addedClasses.size} classes to detect scripts added during code evaluation.")
        addedClasses.foreach(rt => consider(rt, None))
      } catch {
        case NonFatal(t) =>
          log.error("Class scanning after code evaluation failed", t)
          if (thrown != null) thrown.addSuppressed(t)
          else throw t
      }
    }
  }

  private def consider(rt: ReferenceType, thread: Option[ThreadReference]): Unit = {
    if (hasInitiatedClassScanning) {
      // A new class, added after we have initiated scanning
      considerReferenceType(thread, rt)
    } else if (relevantForPostponingClassScan(rt)) {
      bumpScanTimer()
    }
  }

  private def checkIfWereDone(): Unit = {
    val mandatoryTypes = wantedTypes.filter(_._2).keys
    if (mandatoryTypes.forall(foundWantedTypes.contains) && classesToScan.isEmpty) {
      actionsOnComplete.foreach(_.apply())
      actionsOnComplete = List.empty
      subject.onComplete()
    }

  }

  private def doBeforeDone(f: () => Unit): Unit = {
    actionsOnComplete = f :: actionsOnComplete
  }

  private def considerReferenceType(thread: Option[ThreadReference], refType: ReferenceType): Unit = {
    val className = refType.name()

    if (wantedTypes.contains(className)) {
      refType match {
        case ct: ClassType =>
          log.debug(s"Found the $className type")
          foundWantedTypes += className -> ct

          // Record execution of any function associated with the type
          val act = actionPerWantedType.get(className)
          doBeforeDone(() => act.foreach(_.apply(ct)))

          checkIfWereDone()

        case other =>
          log.warn(s"Found the $className type but it's a ${other.getClass.getName} rather than a ClassType")
      }
    } else {
      def callback(maybeIdentifiedScript: Option[IdentifiedScript], lineLocations: Seq[Location]) = maybeIdentifiedScript match {
        case Some(identifiedScript) =>
          try {
            val script = scripts.suggest(identifiedScript.script)
            val bls = breakableLocations.add(script, lineLocations)
            activeBreakpoints.addBreakableLocations(script, bls)
            scriptPublisher.publish(script)
          } catch {
            case NonFatal(t) =>
              log.error("Script publish failure", t)
          }
        case None => // noop, assume logged elsewhere
      }

      scriptFactory.considerReferenceType(thread, refType, callback)

        //TODO: What about this failure case when we had a Future?
//        case Failure(t) =>
//          log.error("Script reference type error", t)
    }
  }

  def setup(observer: Observer[ScanAction]): Unit = {
    log.debug("ClassScanner setup")
    subject.subscribe(observer) //TODO: What to do about the subscription?

    def createRequest(classFilter: String) = {
      val request = virtualMachine.eventRequestManager().createClassPrepareRequest()
      request.addClassFilter(classFilter)
      request.enable()
    }

    NashornDebuggerHost.wantedTypes.keys.foreach(createRequest)
    createRequest("jdk.nashorn.internal.scripts.*")

    bumpScanTimer()
  }

  private def bumpScanTimer(): Unit = {
    scanTimer.foreach(_.cancel())
    scanTimer = Some(DelayedFuture(200.millis) {
      subject.onNext(ScanMoreLater(() => scanClasses()))
    })
  }

  private def scanClasses(): Unit = {
    val referenceTypes = virtualMachine.allClasses()

    classesToScan = referenceTypes.toList

    hasInitiatedClassScanning = true
    totalScanCount = classesToScan.size

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
        log.debug(s"Scanned $count classes in ${elapsed.toMillis} ms, temporarily yielding to other operations. ${classesToScan.size} classes left.")
        subject.onNext(ScanMoreLater(() => scanOutstandingClasses()))
        done = true
      }
    }

    if (classesToScan.isEmpty) {
      log.info(s"Class scanning complete! Scanned $totalScanCount classes in total.")
      checkIfWereDone()
    }
  }


  private def relevantForPostponingClassScan(referenceType: ReferenceType) = {
    val name = referenceType.name()
    wantedTypes.contains(name) || name.startsWith(ScriptClassNamePrefix)
  }

  def typeByName(name: String): Option[ClassType] = foundWantedTypes.get(name).orElse {
    virtualMachine.classByName(name).collect { case ct: ClassType => ct }
  }
}
