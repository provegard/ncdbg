package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host.{ProfilingData, Sample, SampleType}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.mockito.Mockito._
import org.scalatest.Inside

import scala.concurrent.duration._

class ProfilerTest extends UnitTest with DomainActorTesting with ProfilerTesting with Inside {

  "Profiler" - {
    "setSamplingInterval" - {
      "should be accepted" in {
        val profiler = newActorInstance[Profiler]
        requestAndReceiveResponse(profiler, "1", Domain.enable)
        val r = requestAndReceiveResponse(profiler, "2", Profiler.setSamplingInterval(1000))
        r should be (Accepted)
      }
    }

    "start" - {
      "should use the sampling interval in calling the host" in {
        val profiler = newActorInstance[Profiler]
        requestAndReceiveResponse(profiler, "1", Domain.enable)
        requestAndReceiveResponse(profiler, "2", Profiler.setSamplingInterval(1500))
        requestAndReceiveResponse(profiler, "3", Profiler.start)

        verify(currentScriptHost).startProfiling(1500.micros)
      }

      "should reject profile start without sampling interval" in {
        val profiler = newActorInstance[Profiler]
        requestAndReceiveResponse(profiler, "1", Domain.enable)
        val ex = intercept[ResponseException](requestAndReceiveResponse(profiler, "2", Profiler.start))
        ex.getMessage should include ("Sampling interval")
      }

      "should support changing the sampling interval" in {
        val profiler = newActorInstance[Profiler]
        requestAndReceiveResponse(profiler, "1", Domain.enable)
        requestAndReceiveResponse(profiler, "2", Profiler.setSamplingInterval(1500))
        requestAndReceiveResponse(profiler, "3", Profiler.setSamplingInterval(500))
        requestAndReceiveResponse(profiler, "4", Profiler.start)

        verify(currentScriptHost).startProfiling(500.micros)
      }
    }

    "stop" - {
      val stackFrame = createStackFrame("a", "file:///some/script.js", "myFun", 1, 1)
      val sample = Sample(2000L, Seq(stackFrame), SampleType.Script)
      val input = ProfilingData(Seq(sample), 1000, 5000)

      "should use the sampling interval in calling the host" in {
        when(currentScriptHost.stopProfiling()).thenReturn(input)

        val profiler = newActorInstance[Profiler]
        requestAndReceiveResponse(profiler, "1", Domain.enable)
        requestAndReceiveResponse(profiler, "2", Profiler.setSamplingInterval(1500))
        requestAndReceiveResponse(profiler, "3", Profiler.start)
        val response = requestAndReceiveResponse(profiler, "4", Profiler.stop)

        inside(response) {
          case Profiler.ProfileResult(profile) =>
            profile.samples should have size (1)
        }
      }

    }
  }
}
