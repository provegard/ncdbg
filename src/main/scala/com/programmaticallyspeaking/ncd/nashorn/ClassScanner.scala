package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.infra.{CancellableFuture, DelayedFuture}
import com.programmaticallyspeaking.ncd.messaging.{Observer, SerializedSubject}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{ScriptClassNamePrefix, wantedTypes}
import com.sun.jdi.event.ClassPrepareEvent
import com.sun.jdi.request.EventRequest
import com.sun.jdi.{ClassType, ReferenceType, ThreadReference, VirtualMachine}
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

    try f finally {
      val addedClasses = classTracker.addedClasses()
      try {
        log.debug(s"Scanning ${addedClasses.size} classes to detect scripts added during code evaluation.")
        addedClasses.foreach(rt => consider(rt, None))
      } catch {
        case NonFatal(t) =>
          log.error("Class scanning after code evaluation failed", t)
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
            subject.onComplete()
          }

        case other =>
          log.warn(s"Found the $className type but it's a ${other.getClass.getName} rather than a ClassType")
      }
    } else {
      def callback(maybeIdentifiedScript: Option[IdentifiedScript]) = maybeIdentifiedScript match {
        case Some(identifiedScript) =>
          try {
            val script = scripts.suggest(identifiedScript.script)
            val bls = breakableLocations.add(script, refType.allLineLocations().asScala)
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
    subject.subscribe(observer) //TODO: What to do about the subscription?

    val request = virtualMachine.eventRequestManager().createClassPrepareRequest()
    request.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    request.setEnabled(true)

    bumpScanTimer()
  }

  private def bumpScanTimer(): Unit = {
    scanTimer.foreach(_.cancel())
    scanTimer = Some(DelayedFuture(200.millis)(scanClasses()))
  }

  private def scanClasses(): Unit = {
    val referenceTypes = virtualMachine.allClasses()

    hasInitiatedClassScanning = true

    classesToScan = referenceTypes.toList

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
        subject.onNext(ScanMoreLater(scanOutstandingClasses))
        done = true
      }
    }

    if (classesToScan.isEmpty) {
      log.info("Class scanning complete!")
    }
  }


  private def relevantForPostponingClassScan(referenceType: ReferenceType) = {
    val name = referenceType.name()
    wantedTypes.contains(name) || name.startsWith(ScriptClassNamePrefix)
  }

  def typeByName(name: String) = foundWantedTypes.get(name)
}
