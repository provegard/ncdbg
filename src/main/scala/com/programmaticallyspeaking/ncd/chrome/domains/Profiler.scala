package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host.ScriptHost

import scala.concurrent.duration._


object Profiler {

  case object start

  case object stop

  case class setSamplingInterval(interval: Int)

  case class StopResult()

  case class ProfileNode(id: Int, callFrame: Runtime.CallFrame, hitCount: Int, children: Seq[Int])

  case class Profile(nodes: Seq[ProfileNode], startTime: Long, endTime: Long, samples: Seq[Int], timeDeltas: Seq[Int]) {
    def averageTimeDelta: FiniteDuration = (timeDeltas.sum / timeDeltas.size.asInstanceOf[Double]).micros
  }

  case class ProfileResult(profile: Profile)

  object Profile extends ProfileBuilder
}

class Profiler(scriptHost: ScriptHost, eventEmitHook: EventEmitHook) extends DomainActor(scriptHost, eventEmitHook) {
  import Profiler._

  private var currentSamplingInterval: Option[FiniteDuration] = None

  override protected def handle: PartialFunction[AnyRef, Any] = {
    case Profiler.setSamplingInterval(interval) =>
      currentSamplingInterval = Some(interval.micros)

    case Profiler.start =>
      currentSamplingInterval match {
        case Some(interval) =>
          scriptHost.startProfiling(interval)

        case None =>
          throw new IllegalArgumentException("Sampling interval must be set before starting")
      }

    case Profiler.stop =>
      val profile = Profile.fromProfileData(scriptHost.stopProfiling())
      val averageTimeDelta = profile.averageTimeDelta
      currentSamplingInterval.find(_< averageTimeDelta).foreach { d =>
        log.warn(s"Requested sampling interval was ${d.toMicros} us, actual average time delta is ${averageTimeDelta.toMicros} us")
      }
      ProfileResult(profile)
  }
}
