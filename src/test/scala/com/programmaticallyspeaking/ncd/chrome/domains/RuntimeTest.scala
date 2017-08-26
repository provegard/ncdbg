package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime._
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock

import scala.util.{Success, Try}

class RuntimeTest extends UnitTest with DomainActorTesting {

  def objectWithId(id: String) = ObjectNode("Object", ObjectId(id))

  def evaluateOnStackFrameArgs: EvaluateOnStackFrameArgs = {
    val sidCaptor = ArgumentCaptor.forClass(classOf[String])
    val exprCaptor = ArgumentCaptor.forClass(classOf[String])
    val mapCaptor = ArgumentCaptor.forClass(classOf[Map[String, ObjectId]])
    verify(currentScriptHost).evaluateOnStackFrame(sidCaptor.capture(), exprCaptor.capture(), mapCaptor.capture())
    EvaluateOnStackFrameArgs(sidCaptor.getValue, exprCaptor.getValue, mapCaptor.getValue)
  }

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
      val arbitraryObjectId = ObjectId("x")
      val arbitraryObjectIdStr = arbitraryObjectId.toString

      def testGet2(propsById: Map[String, Either[Exception, Map[String, ObjectPropertyDescriptor]]], requestedId: String, own: Option[Boolean], accessor: Option[Boolean],
                   generatePreview: Option[Boolean] = None)(fun: (Any) => Unit) = {

        when(currentScriptHost.getObjectProperties(any[ObjectId], any[Boolean], any[Boolean])).thenAnswer((invocation: InvocationOnMock) => {
          val objId = invocation.getArgument[ObjectId](0)
          propsById.get(objId.toString) match {
            case Some(Right(value)) => value.toSeq
            case Some(Left(ex)) => throw ex
            case None => throw new IllegalArgumentException("Unknown object Id: " + objId.id)
          }
        })

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.getProperties(requestedId, own, accessor, generatePreview))
        fun(response)
      }

      def testGet(ret: Either[Exception, Map[String, ObjectPropertyDescriptor]], requestedId: String, own: Option[Boolean], accessor: Option[Boolean],
                  generatePreview: Option[Boolean] = None)(fun: (Any) => Unit) = {

        testGet2(Map(requestedId -> ret), requestedId, own, accessor, generatePreview)(fun)
      }

      "should call getObjectProperties on the host" in {
        testGet(Right(Map.empty), arbitraryObjectIdStr, None, None) { _ =>
          verify(currentScriptHost).getObjectProperties(arbitraryObjectId, false, false)
        }
      }

      "should call getObjectProperties on the host with own-properties request" in {
        testGet(Right(Map.empty), arbitraryObjectIdStr, Some(true), None) { _ =>
          verify(currentScriptHost).getObjectProperties(arbitraryObjectId, true, false)
        }
      }

      "should call getObjectProperties on the host with accessor-properties request" in {
        testGet(Right(Map.empty), arbitraryObjectIdStr, None, Some(true)) { _ =>
          verify(currentScriptHost).getObjectProperties(arbitraryObjectId, false, true)
        }
      }

      "should return properties in the success case" in {
        testGet(Right(Map.empty), arbitraryObjectIdStr, None, None) { response =>
          response should be (GetPropertiesResult(Seq.empty, None, Seq.empty))
        }
      }

      "should treat [[-named properties as internal" in {
        val props = Map(
          "foo" -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, true, true, true, true, Some(SimpleValue("test")), None, None),
          "[[bar]]" -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, true, true, true, true, Some(SimpleValue("internal")), None, None)
        )
        testGet(Right(props), arbitraryObjectIdStr, None, None) { response =>
          response should be (GetPropertiesResult(
            Seq(PropertyDescriptor("foo", true, true, true, true, Some(RemoteObject.forString("test")), None, None)),
            None,
            Seq(InternalPropertyDescriptor("[[bar]]", Some(RemoteObject.forString("internal")))))
          )
        }
      }

      "should ignore a [[-named properties that is not a data property" in {
        val props = Map(
          "[[bar]]" -> ObjectPropertyDescriptor(PropertyDescriptorType.Accessor, true, true, true, true, None, Some(FunctionNode("fun", "", ObjectId("x"))), None)
        )
        testGet(Right(props), arbitraryObjectIdStr, None, None) {
          case GetPropertiesResult(_, _, internal) =>
            internal should be (Seq.empty)
          case other => fail("Unexpected: " + other)
        }
      }

      "doesn't generate a preview for an internal property, even if requested" in {
        val obj = ObjectNode("Object", ObjectId("obj-1"))
        val propDesc = ObjectPropertyDescriptor(PropertyDescriptorType.Data, true, true, true, true, Some(obj), None, None)
        val props = Map(
          arbitraryObjectIdStr -> Right(Map("[[bar]]" -> propDesc)),
          obj.objectId.toString -> Right(Map.empty[String, ObjectPropertyDescriptor]) // contents are irrelevant
        )
        testGet2(props, arbitraryObjectIdStr, None, None, generatePreview = Some(true)) {
          case GetPropertiesResult(_, _, internal) =>

            val preview = internal.headOption.flatMap(_.value).flatMap(_.preview)
            preview should be ('empty)

          case other => fail("Unexpected: " + other)
        }
      }

      "should return exception details in the failure case" in {
        testGet(Left(new Exception("oops")), arbitraryObjectIdStr, None, None) { response =>
          // Note: Unsure if property descriptors should be empty sequence here or null. The protocol spec doesn't say.
          response should be (GetPropertiesResult(Seq.empty,
            Some(ExceptionDetails(1, s"""Error: 'oops' for object '$arbitraryObjectIdStr'""", 0, 1, None, None, Runtime.StaticExecutionContextId)),
            Seq.empty))
        }
      }

      "should generate preview for a remote object if requested" in {
        val node = objectWithId("x")
        val aMap = Map("foo" -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, true, true, true, true, Some(node), None, None))
        testGet(Right(aMap), arbitraryObjectIdStr, None, None, generatePreview = Some(true)) {
          case GetPropertiesResult(result, _, _) if result.nonEmpty =>
            result.head.value.flatMap(_.preview) should be ('defined)
          case other => fail("Unexpected: " + other)
        }
      }
    }

    "callFunctionOn" - {
      def testCall(obj: ComplexNode, targetId: String, args: Seq[CallArgument], retVal: Option[Try[ValueNode]] = None, silent: Option[Boolean] = None,
                   returnByValue: Option[Boolean] = None, generatePreview: Option[Boolean] = None)(fun: (Any) => Unit) = {

        val actualRetVal = retVal.getOrElse(Success(SimpleValue("ok")))
        when(currentScriptHost.evaluateOnStackFrame(any[String], any[String], any[Map[String, ObjectId]])).thenReturn(actualRetVal)

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.callFunctionOn(targetId, "function(){}", args, silent,
          returnByValue, generatePreview))
        fun(response)
      }

      def testCallArgs(args: Seq[CallArgument])(fun: (EvaluateOnStackFrameArgs) => Unit) = {
        val obj = objectWithId("x")
        testCall(obj, """{"id":"x"}""", args) { _ =>
          fun(evaluateOnStackFrameArgs)
        }
      }

      "should perform ScriptHost evaluation with a wrapped function on the top stack frame and the target as a named object" in {
        val obj = objectWithId("x")
        testCall(obj, """{"id":"x"}""", Seq.empty) { resp =>
          val expr = "(function(){}).apply(__obj_1,[])"
          evaluateOnStackFrameArgs should be (EvaluateOnStackFrameArgs("$top", expr, Map("__obj_1" -> ObjectId("x"))))
        }
      }

      "should generate a preview if requested" in {
        val obj = objectWithId("x")
        val retVal = objectWithId("y")
        testCall(obj, """{"id":"x"}""", Seq.empty, retVal = Some(Success(retVal)), generatePreview = Some(true)) {
          case Runtime.CallFunctionOnResult(result, _) =>
            result.preview should be('defined)
          case other => fail("Unexpected response: " + other)
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

    "evaluate" - {
      def testEvaluate(expr: String, retVal: Option[Try[ValueNode]] = None, silent: Option[Boolean] = None,
                   returnByValue: Option[Boolean] = None, generatePreview: Option[Boolean] = None)(fun: (Any) => Unit) = {

        val actualRetVal = retVal.getOrElse(Success(SimpleValue("ok")))
        when(currentScriptHost.evaluateOnStackFrame(any[String], any[String], any[Map[String, ObjectId]])).thenReturn(actualRetVal)

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.evaluate(expr, None, None, silent, returnByValue,
          generatePreview))
        fun(response)
      }

      def errorValue(msg: String) = ErrorValue(ExceptionData("Exception", msg, 1, 0, "", None), true, ObjectId("err"))

      "should perform ScriptHost evaluation with a wrapped function using null (global) as 'this'" in {
        testEvaluate("42") { _ =>
          val expr = "(function(){return (42);}).call(null);"
          evaluateOnStackFrameArgs should be(EvaluateOnStackFrameArgs("$top", expr, Map.empty))
        }
      }

      "should generate a preview if requested" in {
        val retVal = objectWithId("y")
        testEvaluate("42", generatePreview = Some(true), retVal = Some(Success(retVal))) {
          case Runtime.EvaluateResult(result, _) =>
            result.preview should be('defined)
          case other => fail("Unexpected response: " + other)
        }
      }

      "should report exception by default" in {
        testEvaluate("42", retVal = Some(Success(errorValue("oops")))) {
          case Runtime.EvaluateResult(_, exceptionDetails) =>
            exceptionDetails should be ('defined)
          case other => fail("Unexpected response: " + other)
        }
      }

      "should not report an exception in silent mode" in {
        testEvaluate("42", retVal = Some(Success(errorValue("oops"))), silent = Some(true)) {
          case Runtime.EvaluateResult(result, None) =>
            result should be (RemoteObject.undefinedValue)
          case other => fail("Unexpected response: " + other)
        }
      }
    }

    "when ScriptHost emits UncaughtError" - {
      def testIt = {
        val runtime = newActorInstance[Runtime]
        val ev = ErrorValue(ExceptionData("Error", "oops", 1, 0, "http://some/where", None), true, ObjectId("o1"))
        val req = Messages.Request("1", Domain.enable)
        // First 2 are executionContextCreated and consoleAPICalled
        receiveScriptEventTriggeredEvents(runtime, Seq(req), Seq(UncaughtError(ev)), 3).last
      }
      "should emit Runtime.exceptionThrown event" in {
        val event = testIt
        event.method should be("Runtime.exceptionThrown")
      }
      "should convert the error value" in {
        val event = testIt
        event.params match {
          case Runtime.ExceptionThrownEventParams(ts, exceptionDetails) =>
            exceptionDetails should be (ExceptionDetails(1, "oops", 0, 0, Some("http://some/where")))
          case other => fail("Unexpected: " + other)
        }
      }
    }
  }

  override def createScriptHost(): ScriptHost = {
    val host = super.createScriptHost()

    when(host.getObjectProperties(any[ObjectId], any[Boolean], any[Boolean])).thenReturn(Seq.empty[(String, ObjectPropertyDescriptor)])

    host
  }

  // Helper class for matching against arguments of a mocked call to ScriptHost.evaluateOnStackFrame
  case class EvaluateOnStackFrameArgs(stackFrameId: String, expression: String, namedObjects: Map[String, ObjectId])
}