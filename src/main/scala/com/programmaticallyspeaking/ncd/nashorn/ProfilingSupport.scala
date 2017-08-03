package com.programmaticallyspeaking.ncd.nashorn

import java.util.concurrent.{Executors, ScheduledFuture, TimeUnit}

import com.programmaticallyspeaking.ncd.host.{ProfilingData, Sample, SampleType, ScriptHost}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{isInfrastructureThread, isRunningThread}
import com.sun.jdi.{AbsentInformationException, Location, ThreadReference}
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.control.NonFatal

case class OngoingProfiling(schedule: ScheduledFuture[_], samples: ListBuffer[Sample], startNanos: Long)

trait ProfilingSupport extends ScriptHost { self: NashornDebuggerHost with Logging =>
  import scala.collection.JavaConverters._
  import NashornDebuggerHost._

  private var profiling: Option[OngoingProfiling] = None
  private lazy val profilingExecutor = Executors.newSingleThreadScheduledExecutor()

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

  private def buildStackFramesSequenceForProfiling(locations: Seq[Location], thread: ThreadReference): Seq[StackFrameHolder] = {
    locations.map { location =>
      val functionMethod = location.method()
      val stackframeId = stackframeIdGenerator.next

      try {
        //TODO: Should we include a native frame, e.g. inside NativeInt8Array?
        findActiveBreakpoint(location).map(ab => StackFrameImpl(stackframeId, null, Seq.empty, ab, null, functionDetails(functionMethod))) match {
          case Some(sf) => StackFrameHolder(Some(sf), location)
          case None => StackFrameHolder(None, location)
        }
      } catch {
        case _: AbsentInformationException => StackFrameHolder(None, location)
      }
    }
  }
}
