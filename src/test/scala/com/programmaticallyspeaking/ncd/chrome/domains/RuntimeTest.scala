package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.testing.UnitTest

class RuntimeTest extends UnitTest with DomainActorTesting {

  "Runtime" - {
    "enable" - {
      "should emit an ExecutionContextCreated event" in {
        val runtime = newActorInstance[Runtime]

        val event = requestAndReceiveEvent(runtime, "1", Domain.enable)
        event.method should be("Runtime.executionContextCreated")
      }

      "should emit an ExecutionContextCreated event for a fixed execution context" in {
        val runtime = newActorInstance[Runtime]

        val event = requestAndReceiveEvent(runtime, "1", Domain.enable)
        event.params should be (Runtime.ExecutionContextCreatedEventParams(Runtime.ExecutionContextDescription(1, "top", "top", null)))
      }
    }

    "releaseObjectGroup" - {
      "should be accepted" in {
        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.releaseObjectGroup("foobar"))
        response should be (Accepted)
      }
    }

    "compileScript" - {
      "should return something" in {
        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.compileScript("1+1", "file:///test", false, None))
        response shouldBe a[Runtime.CompileScriptResult]
      }
    }
  }
}
