package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ExceptionDetails, GetPropertiesResult}
import com.programmaticallyspeaking.ncd.host.{ComplexNode, LazyNode, ObjectId, ObjectNode}
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

    "getProperties" - {

      def testGet(obj: ComplexNode, requestedId: String)(fun: (Any) => Unit) = {

        objectsById += (obj.objectId -> obj)

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.getProperties(requestedId, ownProperties = false))
        fun(response)
      }

      "should get an object that exist" in {
        val obj = ObjectNode(Map.empty, ObjectId("x"))
        testGet(obj, """{"id":"x"}""") { response =>
          response should be (GetPropertiesResult(Seq.empty, None))
        }
      }

      "should return an exception for an object that doesn't exist" in {
        val obj = ObjectNode(Map.empty, ObjectId("x"))
        testGet(obj, """{"id":"y"}""") { response =>
          // Note: Unsure if property descriptors should be empty sequence here or null. The protocol spec doesn't say.
          response should be (GetPropertiesResult(Seq.empty,
            Some(ExceptionDetails(1, """Error: Unknown object ID: '{"id":"y"}'""", 0, 1, None, None, Runtime.StaticExecutionContextId))))
        }
      }

      "should return a property value by-reference" in {
        val sub = ObjectNode(Map.empty, ObjectId("a"))
        val obj = ObjectNode(Map("sub" -> LazyNode.eager(sub)), ObjectId("x"))
        testGet(obj, """{"id":"x"}""") {
          case GetPropertiesResult(props, _) =>
            props.headOption.map(_.value.value) should be (Some(null))
          case other => fail("Unknown response: " + other)
        }
      }
    }
  }
}
