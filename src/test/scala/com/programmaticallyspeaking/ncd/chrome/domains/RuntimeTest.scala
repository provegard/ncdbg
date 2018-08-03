package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.Inbox
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime._
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.nashorn.ScriptImpl
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class RuntimeTest extends UnitTest with DomainActorTesting {

  def objectWithId(id: String) = ObjectNode("Object", ObjectId(id))

  def evaluateOnStackFrameArgs: EvaluateOnStackFrameArgs = {
    val sidCaptor = ArgumentCaptor.forClass(classOf[String])
    val exprCaptor = ArgumentCaptor.forClass(classOf[String])
    verify(currentScriptHost).evaluateOnStackFrame(sidCaptor.capture(), exprCaptor.capture())
    EvaluateOnStackFrameArgs(sidCaptor.getValue, exprCaptor.getValue)
  }

  def callFunctionOnArgs: CallFunctionOnArgs = {
    val sidCaptor = ArgumentCaptor.forClass(classOf[String])
    val exprCaptor = ArgumentCaptor.forClass(classOf[String])
    val thisCaptor = ArgumentCaptor.forClass(classOf[Option[ObjectId]])
    val argsCaptor = ArgumentCaptor.forClass(classOf[Seq[ObjectId]])
    verify(currentScriptHost).callFunctionOn(sidCaptor.capture(), thisCaptor.capture(), exprCaptor.capture(), argsCaptor.capture())
    CallFunctionOnArgs(sidCaptor.getValue, thisCaptor.getValue, exprCaptor.getValue, argsCaptor.getValue)
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

      lazy val testCompileScript = {
        val script = new ScriptImpl(ScriptURL.create(""), Array.empty, "xx", ScriptVersion(1, true))
        when(currentScriptHost.compileScript(any[String], any[String], any[Boolean])).thenReturn(Future.successful(Some(script)))

        // Simulate emit from Debugger
        val dummyReceiver = Inbox.create(system).getRef()
        eventEmitHook.emitEvent(Messages.Event("Debugger.scriptParsed", Debugger.ScriptParsedEventParams(script)), dummyReceiver)

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        requestAndReceiveResponse(runtime, "2", Runtime.compileScript("1+1", "file:///test", true, None))
      }

      lazy val testCompileScriptFail = {
        when(currentScriptHost.compileScript(any[String], any[String], any[Boolean])).thenReturn(Future.failed(new Exception("oops")))

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        requestAndReceiveResponse(runtime, "2", Runtime.compileScript("1+1", "file:///test", true, None))
      }

      "invokes the corresponding ScriptHost operation" in {
        testCompileScript

        verify(currentScriptHost).compileScript("1+1", "file:///test", true)
      }

      "returns the script ID" in {
        val response = testCompileScript

        response should be (Runtime.CompileScriptResult("xx", None))
      }

      "reports an error" in {
        val response = testCompileScriptFail

        response match {
          case r: Runtime.CompileScriptResult =>
            exceptionDetailsDescription(r.exceptionDetails) should include ("oops")
          case other => fail("" + other)
        }
      }
    }


    "runScript" - {

      lazy val testRunScript = {
        when(currentScriptHost.runCompiledScript(any[String])).thenReturn(Success(SimpleValue("ok")))

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        requestAndReceiveResponse(runtime, "2", Runtime.runScript("xx", None, false, true))
      }

      def testRunScriptFails() = {
        when(currentScriptHost.runCompiledScript(any[String])).thenReturn(Failure(new Exception("oops")))

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        requestAndReceiveResponse(runtime, "2", Runtime.runScript("xx", None, false, true))
      }

      def testRunScriptReturnsErrorValue(ev: ErrorValue) = {
        when(currentScriptHost.runCompiledScript(any[String])).thenReturn(Success(ev))

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        requestAndReceiveResponse(runtime, "2", Runtime.runScript("xx", None, false, true))
      }

      lazy val testRunScriptFailsResult = testRunScriptFails()

      "converts a thrown error into exception result" in {
        val data = ExceptionData("Error", "oops", 1, 0, "", None)
        val ev = ErrorValue(data, true, ObjectId("x"), None)
        val response = testRunScriptReturnsErrorValue(ev)

        response match {
          case r: Runtime.RunScriptResult =>
            r.exceptionDetails.flatMap(_.exception.flatMap(_.description)) should be (Some("Error: oops"))
          case other => fail("Unexpected response: " + other)
        }
      }

      "invokes the corresponding ScriptHost operation" in {
        testRunScript

        verify(currentScriptHost).runCompiledScript("xx")
      }

      "returns the run result" in {
        val response = testRunScript

        response should be (Runtime.RunScriptResult(RemoteObject.forString("ok"), None))
      }

      "returns undefined value in case of error" in {
        val response = testRunScriptFailsResult

        response match {
          case r: Runtime.RunScriptResult => r.result should be (RemoteObject.undefinedValue)
          case other => fail("" + other)
        }
      }

      "reports an error" in {
        val response = testRunScriptFailsResult

        response match {
          case r: Runtime.RunScriptResult =>
            exceptionDetailsDescription(r.exceptionDetails) should include ("oops")
          case other => fail("" + other)
        }
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
            Some(ExceptionDetails(1, s"""Error: 'oops' for object '$arbitraryObjectIdStr'""", 0, 1, None, None, None, Runtime.StaticExecutionContextId)),
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
      def testCall(target: ComplexNode, args: Seq[CallArgument], retVal: Option[Try[ValueNode]] = None,
                   returnByValue: Option[Boolean] = None, generatePreview: Option[Boolean] = None, functionDecl: Option[String] = None)(fun: (Any) => Unit) = {

        val actualRetVal = retVal.getOrElse(Success(SimpleValue("ok")))
        when(currentScriptHost.callFunctionOn(any[String], any[Option[ObjectId]], any[String], any[Seq[ObjectId]])).thenReturn(actualRetVal)

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val functionDeclaration = functionDecl.getOrElse("function(){}")
        val response = requestAndReceiveResponse(runtime, "2", Runtime.callFunctionOn(target.objectId.toString, functionDeclaration, args,
          returnByValue, generatePreview))
        fun(response)
      }

      def testCallArgs(args: Seq[CallArgument])(fun: (CallFunctionOnArgs) => Unit) = {
        val obj = objectWithId("x")
        testCall(obj, args) { _ =>
          fun(callFunctionOnArgs)
        }
      }

      "should perform ScriptHost evaluation with a wrapped function on the top stack frame and the target as a named object" in {
        val obj = objectWithId("x")
        testCall(obj, Seq.empty) { _ =>
          val expr = "function(){}"
          callFunctionOnArgs should be (CallFunctionOnArgs(StackFrame.TopId, Some(obj.objectId), expr, Seq.empty))
        }
      }

      "should accept null arguments (VSCode may omit the arguments)" in {
        val obj = objectWithId("x")
        testCall(obj, null) { _ =>
          val expr = "function(){}"
          callFunctionOnArgs should be (CallFunctionOnArgs(StackFrame.TopId, Some(obj.objectId), expr, Seq.empty))
        }
      }

      "should transpile a generator function" in {
        val obj = objectWithId("x")
        testCall(obj, Seq.empty, functionDecl = Some("function *gen() { yield 42; }")) { resp =>
          val codePassedToHost = callFunctionOnArgs.funcDecl
          codePassedToHost shouldNot include ("yield")
        }
      }

      "should generate a preview if requested" in {
        val obj = objectWithId("x")
        val retVal = objectWithId("y")
        testCall(obj, Seq.empty, retVal = Some(Success(retVal)), generatePreview = Some(true)) {
          case Runtime.CallFunctionOnResult(result, _) =>
            result.preview should be('defined)
          case other => fail("Unexpected response: " + other)
        }
      }

      "should support a call argument that is a plain value" in {
        val arg = CallArgument(Some("test"), None, None)
        testCallArgs(Seq(arg)) { result =>
          result.funcDecl should include ("[\"test\"]")
        }
      }

      "should support a call argument that is an unserializable value" in {
        val arg = CallArgument(None, Some("NaN"), None)
        testCallArgs(Seq(arg)) { result =>
          result.funcDecl should include ("[NaN]")
        }
      }

      "should support a call argument that is an object ID" in {
        val arg = CallArgument(None, None, Some("""{"id":"foo"}"""))
        testCallArgs(Seq(arg)) { result =>
          result.args should contain (ObjectId("foo"))
        }
      }

      "should support a call argument that is undefined" in {
        val arg = CallArgument(None, None, None)
        testCallArgs(Seq(arg)) { result =>
          result.funcDecl should include ("[void 0]")
        }
      }

      "should support multiple call arguments" in {
        val arg1 = CallArgument(Some("test"), None, None)
        val arg2 = CallArgument(Some(42), None, None)
        testCallArgs(Seq(arg1, arg2)) { result =>
          result.funcDecl should include ("[\"test\",42]")
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
      def testEvaluate(expr: String, retVal: Option[Try[ValueNode]] = None,
                   returnByValue: Option[Boolean] = None, generatePreview: Option[Boolean] = None)(fun: (Any) => Unit) = {

        val actualRetVal = retVal.getOrElse(Success(SimpleValue("ok")))
        when(currentScriptHost.evaluateOnStackFrame(any[String], any[String])).thenReturn(actualRetVal)

        val runtime = newActorInstance[Runtime]
        requestAndReceive(runtime, "1", Domain.enable)
        val response = requestAndReceiveResponse(runtime, "2", Runtime.evaluate(expr, None, None, returnByValue,
          generatePreview))
        fun(response)
      }

      def errorValue(msg: String) = ErrorValue(ExceptionData("Exception", msg, 1, 0, "", None), true, ObjectId("err"), None)

      "should return user agent directly (VS Code asks)" in {
        testEvaluate("navigator.userAgent") {
          case Runtime.EvaluateResult(result, _) =>
            val value = result.value.getOrElse("Missing response value in " + result)
            value.toString should startWith ("NCDbg version")
          case other => fail("Unexpected response: " + other)
        }
      }

      "should perform ScriptHost evaluation with a wrapped function using null (global) as 'this'" in {
        testEvaluate("42") { _ =>
          val expr = "(function(){return (42);}).call(null);"
          evaluateOnStackFrameArgs should be(EvaluateOnStackFrameArgs(StackFrame.TopId, expr))
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
    }

    "when ScriptHost emits UncaughtError" - {
      def testIt = {
        val runtime = newActorInstance[Runtime]
        val ev = ErrorValue(ExceptionData("Error", "oops", 1, 0, "http://some/where", None), true, ObjectId("o1"), None)
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
            val exc = RemoteObject.forError("Error", "oops", Some("Error: oops"), """{"id":"o1"}""")
            exceptionDetails should be (ExceptionDetails(1, "Uncaught", 0, 0, Some("http://some/where"), exception = Some(exc)))
          case other => fail("Unexpected: " + other)
        }
      }
    }


    "when ScriptHost emits PrintMessage" - {
      def testIt(msg: String) = {
        val runtime = newActorInstance[Runtime]
        val req = Messages.Request("1", Domain.enable)
        // First 2 are executionContextCreated and consoleAPICalled
        receiveScriptEventTriggeredEvents(runtime, Seq(req), Seq(PrintMessage(msg)), 3).last
      }
      "should emit Runtime.consoleAPICalled event" in {
        val event = testIt("hello")
        event.method should be("Runtime.consoleAPICalled")
      }

      "should use log level in Runtime.consoleAPICalled" in {
        withConsoleEventParams(testIt("world")) { p =>
          p.`type` should be ("log")
        }
      }

      "should pass the actual message in Runtime.consoleAPICalled" in {
        withConsoleEventParams(testIt("world")) { p =>
          p.args should be (Seq(RemoteObject.forString("world")))
        }
      }

    }
  }

  private def exceptionDetailsDescription(ed: Option[ExceptionDetails]): String =
    ed.flatMap(_.exception.flatMap(_.description)).getOrElse("")

  private def withConsoleEventParams(event: Messages.Event)(f: ConsoleAPICalledEventParams => Unit): Unit = {
    event.params match {
      case p: Runtime.ConsoleAPICalledEventParams => f(p)
      case other => fail("Unexpected: " + other)
    }
  }

  override def createScriptHost(): ScriptHost = {
    val host = super.createScriptHost()

    when(host.getObjectProperties(any[ObjectId], any[Boolean], any[Boolean])).thenReturn(Seq.empty[(String, ObjectPropertyDescriptor)])

    host
  }

  // Helper class for matching against arguments of a mocked call to ScriptHost.evaluateOnStackFrame
  case class EvaluateOnStackFrameArgs(stackFrameId: String, expression: String)

  // Helper class for matching against arguments of a mocked call to ScriptHost.callFunctionOn
  case class CallFunctionOnArgs(stackFrameId: String, thisObject: Option[ObjectId], funcDecl: String, args: Seq[ObjectId])
}