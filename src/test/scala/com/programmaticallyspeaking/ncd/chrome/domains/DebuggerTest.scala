package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.PoisonPill
import akka.testkit.TestProbe
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.PausedEventParams
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ExceptionDetails, RemoteObject}
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.ExceptionData
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.Inside
import org.scalatest.concurrent.Eventually

import scala.collection.mutable
import scala.util.Try

class DebuggerTest extends UnitTest with DomainActorTesting with Inside with Eventually {
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

      "with a scope object" - {
        val thisObj = ObjectNode(Map.empty, ObjectId("$$this"))
        val scopeObj = ObjectNode(Map.empty, ObjectId("$$scope"))
        val stackFrame = createStackFrame("sf1", thisObj, Some(scopeObj), Map.empty, Breakpoint("bp1", "a", 10), "fun")

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

        "should have a local scope in the event params" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.callFrames.head.scopeChain.map(_.`type`) should contain ("local")
        }

        "should have the correct function name in the call frame" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.callFrames.head.functionName should be ("fun")
        }
      }

      "without a scope object" - {
        val thisObj = ObjectNode(Map.empty, ObjectId("$$this"))
        val stackFrame = createStackFrame("sf1", thisObj, None, Map.empty, Breakpoint("bp1", "a", 10), "fun")

        "should not have a closure scope in the event params" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.callFrames.head.scopeChain.map(_.`type`) shouldNot contain ("closure")
        }

        "should have a local scope in the event params" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.callFrames.head.scopeChain.map(_.`type`) should contain ("local")
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

      def testEvalHandling(theAnswer: => ValueNode, silent: Option[Boolean] = None)(fun: (Debugger.EvaluateOnCallFrameResult) => Unit): Unit = {
        when(currentScriptHost.evaluateOnStackFrame(any[String], any[String])).thenAnswer(new Answer[Try[ValueNode]] {
          override def answer(invocation: InvocationOnMock): Try[ValueNode] = {
            Try(theAnswer)
          }
        })

        val debugger = newActorInstance[Debugger]
        debugger ! Messages.Request("1", Domain.enable)

        inside(requestAndReceiveResponse(debugger, "", Debugger.evaluateOnCallFrame("a", "5+5", silent))) {
          case r: Debugger.EvaluateOnCallFrameResult =>
            fun(r)
        }
      }

      "should return a response if ScriptHost evaluation is successful" in {
        testEvalHandling(SimpleValue("success")) { resp =>
          resp.result should be (RemoteObject.forString("success"))
        }
      }

      "should return a response with value 'undefined' when ScriptHost evaluation returns an error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, ObjectId("$$err"))) { resp =>
          resp.result should be (RemoteObject.undefinedValue)
        }
      }

      "should return a response with exception details when ScriptHost evaluation returns an error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, ObjectId("$$err"))) { resp =>
          // Remember, Chrome line numbers are 0-based, so 10 => 9
          resp.exceptionDetails should be (Some(ExceptionDetails(1, "oops", 9, 1, Some("<eval>"), None, Runtime.StaticExecutionContextId)))
        }
      }

      "should return a response with _only_ value 'undefined' when ScriptHost evaluation returns an error value but silent mode is requested" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, ObjectId("$$err")), Some(true)) { resp =>
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
    })


    host
  }

  def createStackFrame(Aid: String, AthisObj: ValueNode, AscopeObj: Option[ValueNode], Alocals: Map[String, ValueNode], Abreakpoint: Breakpoint, functionName: String) = new StackFrame {
    override val locals: ObjectNode = ObjectNode(Alocals.map(e => e._1 -> LazyNode.eager(e._2)), ObjectId("$$locals"))
    override val breakpoint: Breakpoint = Abreakpoint
    override val thisObj: ValueNode = AthisObj
    override val scopeObj: Option[ValueNode] = AscopeObj
    override val id: String = Aid
    override val functionDetails: FunctionDetails = FunctionDetails(functionName)
  }
}
