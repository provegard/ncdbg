package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{CallArgument, ExceptionDetails, GetPropertiesResult}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.mockito.ArgumentCaptor
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._

import scala.util.{Success, Try}

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

    "callFunctionOn" - {
      def testCall(obj: ComplexNode, targetId: String, args: Seq[CallArgument], retVal: Option[Try[ValueNode]] = None, silent: Option[Boolean] = None, returnByValue: Option[Boolean] = None)(fun: (Any) => Unit) = {

        objectsById += (obj.objectId -> obj)

        val actualRetVal = retVal.getOrElse(Success(SimpleValue("ok")))
        when(currentScriptHost.evaluateOnStackFrame(any[String], any[String], any[Map[String, ObjectId]])).thenReturn(actualRetVal)

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.callFunctionOn(targetId, "function(){}", args, silent, returnByValue))
        fun(response)
      }

      def evaluateOnStackFrameArgs: EvaluateOnStackFrameArgs = {
        val sidCaptor = ArgumentCaptor.forClass(classOf[String])
        val exprCaptor = ArgumentCaptor.forClass(classOf[String])
        val mapCaptor = ArgumentCaptor.forClass(classOf[Map[String, ObjectId]])
        verify(currentScriptHost).evaluateOnStackFrame(sidCaptor.capture(), exprCaptor.capture(), mapCaptor.capture())
        EvaluateOnStackFrameArgs(sidCaptor.getValue, exprCaptor.getValue, mapCaptor.getValue)
      }

      def testCallArgs(args: Seq[CallArgument])(fun: (EvaluateOnStackFrameArgs) => Unit) = {
        val obj = ObjectNode(Map.empty, ObjectId("x"))
        testCall(obj, """{"id":"x"}""", args) { _ =>
          fun(evaluateOnStackFrameArgs)
        }
      }

      "should perform ScriptHost evaluation with a wrapped function on the top stack frame and the target as a named object" in {
        val obj = ObjectNode(Map.empty, ObjectId("x"))
        testCall(obj, """{"id":"x"}""", Seq.empty) { resp =>
          val expr = "(function(){}).apply(__obj_1,[])"
          evaluateOnStackFrameArgs should be (EvaluateOnStackFrameArgs("$top", expr, Map("__obj_1" -> ObjectId("x"))))
        }
      }

      "should support a call argument that is a plain value" in {
        val arg = CallArgument(Some("test"), None, None)
        testCallArgs(Seq(arg)) { result =>
          result.expression should include ("[\"test\"]")
        }
      }

      "should support a call argument that is an unserializable value" in {
        val arg = CallArgument(None, Some("NaN"), None)
        testCallArgs(Seq(arg)) { result =>
          result.expression should include ("[NaN]")
        }
      }

      "should support a call argument that is an object ID" in {
        val arg = CallArgument(None, None, Some("""{"id":"foo"}"""))
        testCallArgs(Seq(arg)) { result =>
          result.expression should include ("[__obj_2]")
        }
      }

      "should provide a name-object ID mapping when a call argument is an object ID" in {
        val arg = CallArgument(None, None, Some("""{"id":"foo"}"""))
        testCallArgs(Seq(arg)) { result =>
          result.namedObjects should contain ("__obj_2" -> ObjectId("foo"))
        }
      }

      "should support a call argument that is undefined" in {
        val arg = CallArgument(None, None, None)
        testCallArgs(Seq(arg)) { result =>
          result.expression should include ("[undefined]")
        }
      }

      "should support multiple call arguments" in {
        val arg1 = CallArgument(Some("test"), None, None)
        val arg2 = CallArgument(Some(42), None, None)
        testCallArgs(Seq(arg1, arg2)) { result =>
          result.expression should include ("[\"test\",42]")
        }
      }
    }

    "runIfWaitingForDebugger" - {
      "should be supported" in {
        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.runIfWaitingForDebugger)
        response should be (Accepted)
      }
    }

    "releaseObject" - {
      "should be supported" in {
        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.releaseObject("x"))
        response should be (Accepted)
      }
    }
  }

  // Helper class for matching against arguments of a mocked call to ScriptHost.evaluateOnStackFrame
  case class EvaluateOnStackFrameArgs(stackFrameId: String, expression: String, namedObjects: Map[String, ObjectId])
}