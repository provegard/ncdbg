package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.PoisonPill
import akka.testkit.TestProbe
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.PausedEventParams
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ExceptionDetails, RemoteObject}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, ObjectPropertyDescriptor}
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.Inside
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.mutable
import scala.util.Try

class DebuggerTest extends UnitTest with DomainActorTesting with Inside with Eventually with TableDrivenPropertyChecks {
  import org.mockito.Mockito._
  import org.mockito.ArgumentMatchers._
  import com.programmaticallyspeaking.ncd.testing.MockingUtils._

  def script(theId: String): Script = new Script {
    override def contentsHash(): String = "xyz"
    override val uri: String = theId
    override val lineCount: Int = 5
    override val lastLineLength: Int = 10
    override val contents: String = "abc"
    override val id: String = theId
  }

  "Debugger" - {
    "enable" - {
      "should emit a ScriptParsed event for a script" in {
        addScript(script("a"))
        val debugger = newActorInstance[Debugger]

        val event = requestAndReceiveEvent(debugger, "1", Domain.enable)
        event.method should be ("Debugger.scriptParsed")
      }
      //TODO: assert event details

      "should tell the host to pause on breakpoints" in {
        val debugger = newActorInstance[Debugger]

        requestAndReceiveResponse(debugger, "1", Domain.enable)

        verify(currentScriptHost).pauseOnBreakpoints()
      }
    }

    "setBreakpointsActive" - {
      "should tell the host to pause on breakpoints if active" in {
        val debugger = newActorInstance[Debugger]
        requestAndReceiveResponse(debugger, "1", Domain.enable)

        // Clear invocations since Domain.enable generates a call to setBreakpointsActive also
        clearInvocations(currentScriptHost)

        requestAndReceiveResponse(debugger, "2", Debugger.setBreakpointsActive(true))
        verify(currentScriptHost).pauseOnBreakpoints()
      }

      "should tell the host to ignore breakpoints if not active" in {
        val debugger = newActorInstance[Debugger]
        requestAndReceiveResponse(debugger, "1", Domain.enable)
        requestAndReceiveResponse(debugger, "2", Debugger.setBreakpointsActive(false))
        verify(currentScriptHost).ignoreBreakpoints()
      }
    }

    "getScriptSource" - {
      "should return script contents" in {
        addScript(script("a")) //TODO: specify contents here to make the test clearer
        val debugger = newActorInstance[Debugger]

        debugger ! Messages.Request("1", Domain.enable)
        inside(requestAndReceiveResponse(debugger, "2", Debugger.getScriptSource("a"))) {
          case Debugger.GetScriptSourceResult(src) =>
            src should be ("abc")
        }
      }
    }

    "setBreakpointByUrl" - {

      def setBreakpointByUrl(lineNumber: Int, scriptUri: String): Debugger.SetBreakpointByUrlResult = {
        addScript(script("a")) //TODO: specify line number range here to make the test clearer
        val debugger = newActorInstance[Debugger]

        debugger ! Messages.Request("1", Domain.enable)
        inside(requestAndReceiveResponse(debugger, "2", Debugger.setBreakpointByUrl(lineNumber, scriptUri, 0, ""))) {
          case result: Debugger.SetBreakpointByUrlResult => result
        }
      }

      "should invoke ScriptHost with line number translation from 0-based to 1-based" in {
        val result = setBreakpointByUrl(3, "a")
        result.breakpointId should be ("bp_4")
      }

      "should return a result with a single location" in {
        val result = setBreakpointByUrl(3, "a")
        result.locations.size should be (1)
      }

      "should return a result with the correct script ID" in {
        val result = setBreakpointByUrl(3, "a")
        result.locations.headOption.map(_.scriptId) should be (Some("a_id"))
      }

      "should return a result with the correct 0-based line number" in {
        val result = setBreakpointByUrl(3, "a")
        result.locations.headOption.map(_.lineNumber) should be (Some(3))
      }
    }

    "removeBreakpoint" - {

      "should invoke ScriptHost" in {
        val debugger = newActorInstance[Debugger]
        debugger ! Messages.Request("1", Domain.enable)

        val result = requestAndReceiveResponse(debugger, "2", Debugger.setBreakpointByUrl(5, "a", 0, "")) match {
          case result: Debugger.SetBreakpointByUrlResult => result
          case other => fail("Unexpected: " + other)
        }
        val breakpointId = result.breakpointId

        requestAndReceiveResponse(debugger, "3", Debugger.removeBreakpoint(breakpointId))

        activeBreakpoints shouldNot contain (breakpointId)
      }
    }

    "HitBreakpoint handling" - {
      def simulateHitBreakpoint(stackFrames: Seq[StackFrame]): Messages.Event = {
        val debugger = newActorInstance[Debugger]

        receiveScriptEventTriggeredEvent(debugger, Seq(Messages.Request("1", Domain.enable)),
          Seq(HitBreakpoint(stackFrames))
        )
      }
      def getEventParams(ev: Messages.Event): PausedEventParams = ev.params match {
        case p: PausedEventParams => p
        case other => throw new IllegalArgumentException("Unknown params: " + other)
      }

      val scopeTests = Table(
        ("scope type", "expected type"),
        (ScopeType.Closure, "closure"),
        (ScopeType.Global, "global"),
        (ScopeType.With, "with"),
        (ScopeType.Local, "local")
      )

      forAll(scopeTests) { (scopeType, expected) =>
        s"translates scope of type ${scopeType.getClass.getSimpleName} correctly" in {
          val thisObj = ObjectNode(Map.empty, ObjectId("$$this"))
          val scopeObjectId = ObjectId("$$scope")
          val scopeObj = ObjectNode(Map.empty, scopeObjectId)
          val stackFrame = createStackFrame("sf1", thisObj, Some(Scope(scopeObj, scopeType)), Breakpoint("bp1", "a", 10), "fun")
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)

          params.callFrames.head.scopeChain.find(_.`object`.objectId.contains(scopeObjectId.toString)) match {
            case Some(result) =>
              result.`type` should be (expected)
            case None => fail("Didn't find the scope")
          }

        }
      }

      "with a scope object" - {
        val thisObj = ObjectNode(Map.empty, ObjectId("$$this"))
        val scopeObj = ObjectNode(Map.empty, ObjectId("$$scope"))
        val stackFrame = createStackFrame("sf1", thisObj, Some(Scope(scopeObj, ScopeType.Closure)), Breakpoint("bp1", "a", 10), "fun")

        "should result in a Debugger.paused event" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          ev.method should be("Debugger.paused")
        }

        "should have a call frame in the event params" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.callFrames.size should be (1)
        }

        "should have a closure scope in the event params" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.callFrames.head.scopeChain.map(_.`type`) should contain ("closure")
        }

        "should return the closure scope by-reference" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          val closureScope = params.callFrames.head.scopeChain.find(_.`type` == "closure")
          closureScope.map(_.`object`.value) should be (Some(None))
        }

        "should have the correct function name in the call frame" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.callFrames.head.functionName should be ("fun")
        }
      }

      "without scopes" - {
        val thisObj = ObjectNode(Map.empty, ObjectId("$$this"))
        val stackFrame = createStackFrame("sf1", thisObj, None, Breakpoint("bp1", "a", 10), "fun")

        "should not have a scopes in the event params" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.callFrames.head.scopeChain should be ('empty)
        }
      }
    }

    "Resumed handling" - {
      "should emit a resumed event" in {
        val debugger = newActorInstance[Debugger]

        val ev = receiveScriptEventTriggeredEvent(debugger, Seq(Messages.Request("1", Domain.enable)),
          Seq(Resumed)
        )
        ev.method should be ("Debugger.resumed")
      }
    }

    "evaluateOnCallFrame" - {

      def testEvalHandling(theAnswer: => ValueNode, silent: Option[Boolean] = None, returnByValue: Option[Boolean] = None, generatePreview: Option[Boolean] = None)(fun: (Debugger.EvaluateOnCallFrameResult) => Unit): Unit = {
        when(currentScriptHost.evaluateOnStackFrame(any[String], any[String], any[Map[String, ObjectId]])).thenAnswer(new Answer[Try[ValueNode]] {
          override def answer(invocation: InvocationOnMock): Try[ValueNode] = {
            Try(theAnswer)
          }
        })

        val debugger = newActorInstance[Debugger]
        debugger ! Messages.Request("1", Domain.enable)

        inside(requestAndReceiveResponse(debugger, "", Debugger.evaluateOnCallFrame("a", "5+5", silent, returnByValue, generatePreview))) {
          case r: Debugger.EvaluateOnCallFrameResult =>
            fun(r)
        }
      }

      "should return a response if ScriptHost evaluation is successful" in {
        testEvalHandling(SimpleValue("success")) { resp =>
          resp.result should be (RemoteObject.forString("success"))
        }
      }

      "should return a response with value 'undefined' when ScriptHost evaluation returns a Throwable-based error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None, None)
        testEvalHandling(ErrorValue(exData, isBasedOnThrowable = true, ObjectId("$$err"))) { resp =>
          resp.result should be (RemoteObject.undefinedValue)
        }
      }

      "should return a response with an error value when ScriptHost evaluation returns a non-Throwable-based error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None, None)
        testEvalHandling(ErrorValue(exData, isBasedOnThrowable = false, ObjectId("$$err"))) { resp =>
          resp.result should be (RemoteObject.forError("Error", "oops", None, """{"id":"$$err"}"""))
        }
      }

      "should return a response with exception details when ScriptHost evaluation returns a Throwable-based error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None, None)
        testEvalHandling(ErrorValue(exData, isBasedOnThrowable = true, ObjectId("$$err"))) { resp =>
          // Remember, Chrome line numbers are 0-based, so 10 => 9
          resp.exceptionDetails should be (Some(ExceptionDetails(1, "oops", 9, 1, Some("<eval>"), None, Runtime.StaticExecutionContextId)))
        }
      }

      "should return a response with _only_ value 'undefined' when ScriptHost evaluation returns a Throwable-based error value but silent mode is requested" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None, None)
        testEvalHandling(ErrorValue(exData, isBasedOnThrowable = true, ObjectId("$$err")), Some(true)) { resp =>
          resp should be (Debugger.EvaluateOnCallFrameResult(RemoteObject.undefinedValue, None))
        }
      }

      "should return a response with value 'undefined' when ScriptHost evaluation actually throws" in {
        testEvalHandling(throw new Exception("fail")) { resp =>
          resp.result should be (RemoteObject.undefinedValue)
        }
      }

      "should convert an actual exception into exception details with message" in {
        testEvalHandling(throw new Exception("fail")) { resp =>
          resp.exceptionDetails.map(_.text) should be (Some("fail"))
        }
      }

      "should convert an actual exception into exception details with line number" in {
        testEvalHandling(DebuggerTestHelper.fail("fail")) { resp =>
          // DebuggerTestHelper throws at line 6, which converted to base 0 becomes 5
          resp.exceptionDetails.map(_.lineNumber) should be (Some(5))
        }
      }

      "should convert an actual exception into exception details with URL" in {
        testEvalHandling(DebuggerTestHelper.fail("fail")) { resp =>
          inside(resp.exceptionDetails.flatMap(_.url)) {
            case Some(url) =>
              url should fullyMatch regex ("file:/.*DebuggerTestHelper\\.scala".r)
          }
        }
      }

      "should support by-value return" in {
        val items = Seq(LazyNode.eager(SimpleValue("abc")))
        val arr = ArrayNode(items, ObjectId("x"))
        testEvalHandling(arr, returnByValue = Some(true)) { resp =>
          resp.result should be (RemoteObject.forArray(Seq("abc")))
        }
      }

      "should generate a preview if requested" in {
        val obj = ObjectNode(Map.empty, ObjectId("x"))
        testEvalHandling(obj, generatePreview = Some(true)) { resp =>
          resp.result.preview should be ('defined)
        }
      }

      "should request own object properties and not only accessors when generating a preview" in {
        val obj = ObjectNode(Map.empty, ObjectId("x"))
        testEvalHandling(obj, generatePreview = Some(true)) { resp =>
          verify(currentScriptHost).getObjectProperties(ObjectId("x"), true, false)
        }
      }
    }

    "should tell ScriptHost to reset when stopping" in {
      val probe = TestProbe()
      val debugger = newActorInstance[Debugger]
      probe.watch(debugger)

      requestAndReceive(debugger, "1", Domain.enable)

      debugger ! PoisonPill

      // Wait for Debugger to die
      probe.expectTerminated(debugger)

      verify(currentScriptHost).reset()
    }
  }

  private val activeBreakpoints = mutable.Set[String]()

  override def createScriptHost(): ScriptHost = {
    val host = super.createScriptHost()

    when(host.setBreakpoint(any[String], any[Int])).thenAnswerWith({
      case (uri: String) :: (lineNumber : Integer) :: Nil =>
        val id = "bp_" + lineNumber
        activeBreakpoints += id
        Breakpoint(id, uri + "_id", lineNumber)
    })
    when(host.removeBreakpointById(any[String])).thenAnswerWith({
      case (id: String) :: Nil =>
        activeBreakpoints -= id
        Done
    })
    when(host.getObjectProperties(any[ObjectId], any[Boolean], any[Boolean])).thenReturn(Map.empty[String, ObjectPropertyDescriptor])

    host
  }

  def createStackFrame(Aid: String, AthisObj: ValueNode, scope: Option[Scope], Abreakpoint: Breakpoint, functionName: String) = new StackFrame {
    override val breakpoint: Breakpoint = Abreakpoint
    override val thisObj: ValueNode = AthisObj
    override val scopeChain = scope.toSeq
    override val id: String = Aid
    override val functionDetails: FunctionDetails = FunctionDetails(functionName)
  }
}
