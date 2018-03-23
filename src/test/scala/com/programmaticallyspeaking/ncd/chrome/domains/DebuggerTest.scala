package com.programmaticallyspeaking.ncd.chrome.domains

import java.io.{File, FileNotFoundException}
import java.net.URL
import java.nio.charset.Charset

import akka.actor.{ActorRef, PoisonPill}
import akka.testkit.TestProbe
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.{Scope => _, _}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ExceptionDetails, RemoteObject}
import com.programmaticallyspeaking.ncd.chrome.net.FilePublisher
import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.infra.FileReader
import com.programmaticallyspeaking.ncd.ioc.Container
import com.programmaticallyspeaking.ncd.nashorn.ScriptImpl
import com.programmaticallyspeaking.ncd.testing.{FakeFilePublisher, UnitTest}
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.Inside
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.mutable
import scala.util.{Failure, Success, Try}



class DebuggerTest extends UnitTest with DomainActorTesting with Inside with Eventually with TableDrivenPropertyChecks {
  import org.mockito.Mockito._
  import org.mockito.ArgumentMatchers.{eq => meq, _}
  import com.programmaticallyspeaking.ncd.testing.MockingUtils._

  def script(theId: String, hash: String = "xyz"): Script = new Script {
    override def contentsHash(): String = hash
    override val url: ScriptURL = ScriptURL.create("/tmp/" + theId)
    override val lineCount: Int = 5
    override val lastLineLength: Int = 10
    override val contents: String = "abc"
    override val id: String = theId

    override def sourceMapUrl(): Option[ScriptURL] = None
    override def sourceUrl(): Option[ScriptURL] = None

    override def sourceLine(lineNumber1Based: Int): Option[String] = None
  }

  private val objectProperties = mutable.Map[ObjectId, Map[String, Any]]()

  def setProperties(c: ComplexNode, props: Map[String, Any]): Unit = objectProperties += (c.objectId -> props)

  def location(ln: Int) = ScriptLocation(ln, None)
  def location(ln: Int, cn: Int) = ScriptLocation(ln, Some(cn))

  val setPauseOnExceptionsCases = Table(
    ("state", "expected"),
    ("none", ExceptionPauseType.None),
    ("uncaught", ExceptionPauseType.Uncaught),
    ("all", ExceptionPauseType.All)
  )

  def objectWithId(id: String) = ObjectNode("Object", ObjectId(id))

  "Debugger" - {
    "enable" - {
      "should emit a ScriptParsed event for a script" in {
        addScript(script("a"))
        val debugger = newActorInstance[Debugger]

        val event = requestAndReceiveEvent(debugger, "1", Domain.enable)
        event.method should be ("Debugger.scriptParsed")
      }

      "should tell the host to pause on breakpoints" in {
        val debugger = newActorInstance[Debugger]

        requestAndReceiveResponse(debugger, "1", Domain.enable)

        verify(currentScriptHost).pauseOnBreakpoints()
      }

      "should re-emit ScriptParsed events after disabling and re-enabling" in {
        addScript(script("a"))
        val debugger = newActorInstance[Debugger]

        val e1 = requestAndReceiveEvent(debugger, "1", Domain.enable)
        val scriptId1 = e1.params.asInstanceOf[ScriptParsedEventParams].scriptId

        requestAndReceiveResponse(debugger, "2", Domain.disable)
        val e2 = requestAndReceiveEvent(debugger, "3", Domain.enable)
        val scriptId2 = e2.params.asInstanceOf[ScriptParsedEventParams].scriptId

        scriptId2 should be (scriptId1)
      }
    }

    "setPauseOnExceptions" - {
      forAll(setPauseOnExceptionsCases) { (state, expected) =>
        s"should handle state '$state'" in {
          val debugger = newActorInstance[Debugger]
          requestAndReceiveResponse(debugger, "1", Domain.enable)

          requestAndReceiveResponse(debugger, "2", Debugger.setPauseOnExceptions(state))
          verify(currentScriptHost).pauseOnExceptions(expected)
        }
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

      def setBreakpoint(lineNumber: Int, msg: AnyRef): Debugger.SetBreakpointByUrlResult = {
        addScript(script("a")) //TODO: specify line number range here to make the test clearer
        val debugger = newActorInstance[Debugger]

        debugger ! Messages.Request("1", Domain.enable)
        inside(requestAndReceiveResponse(debugger, "2", msg)) {
          case result: Debugger.SetBreakpointByUrlResult => result
        }
      }
      def setBreakpointByUrl(lineNumber: Int, scriptUri: String, condition: Option[String] = None): Debugger.SetBreakpointByUrlResult = {
        setBreakpoint(lineNumber, Debugger.setBreakpointByUrl(lineNumber, Some(scriptUri), None, None, condition))
      }
      def setBreakpointByUrlRegex(lineNumber: Int, urlRegex: String, condition: Option[String] = None): Debugger.SetBreakpointByUrlResult = {
        setBreakpoint(lineNumber, Debugger.setBreakpointByUrl(lineNumber, None, Some(urlRegex), None, condition))
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

      "should return a breakpoint ID even if the breakpoint location is unknown, to support future-resolved breakpoints" in {
        val result = setBreakpointByUrl(101, "a")
        result.breakpointId should startWith ("bp_")
      }

      "should return no locations if the breakpoint location is unknown" in {
        val result = setBreakpointByUrl(101, "a")
        result.locations should be ('empty)
      }

      "should translate empty condition to no condition" in {
        setBreakpointByUrl(3, "a", Some(""))
        verify(currentScriptHost).setBreakpoint(any[ScriptIdentity], any[ScriptLocation], meq(None))
      }

      "should support the 'urlRegex' parameter" in {
        val result = setBreakpointByUrlRegex(3, "[aA]")
        result.locations.headOption.map(_.scriptId) should be (Some("re_[aA]_id"))
      }
    }

    "removeBreakpoint" - {

      "should invoke ScriptHost" in {
        val debugger = newActorInstance[Debugger]
        debugger ! Messages.Request("1", Domain.enable)

        val result = requestAndReceiveResponse(debugger, "2", Debugger.setBreakpointByUrl(5, Some("a"), None, None, None)) match {
          case result: Debugger.SetBreakpointByUrlResult => result
          case other => fail("Unexpected: " + other)
        }
        val breakpointId = result.breakpointId

        requestAndReceiveResponse(debugger, "3", Debugger.removeBreakpoint(breakpointId))

        activeBreakpoints shouldNot contain (breakpointId)
      }
    }

    "getPossibleBreakpoints" - {

      def setup: ActorRef = {
        when(currentScriptHost.getBreakpointLocations(any[ScriptIdentity], any[ScriptLocation], any[Option[ScriptLocation]]))
          .thenReturn(Seq(location(1, 1), location(2, 1), location(3, 1)))

        val debugger = newActorInstance[Debugger]
        debugger ! Messages.Request("1", Domain.enable)
        debugger
      }

      "should invoke ScriptHost, converting Chrome line/column numbers (0-based) to Nashorn line/column numbers (1-based)" in {
        val debugger = setup
        val start = Location("a", 1, Some(2))
        val end = Location("a", 5, Some(3))

        requestAndReceiveResponse(debugger, "2", Debugger.getPossibleBreakpoints(start, Some(end)))

        verify(currentScriptHost).getBreakpointLocations(ScriptIdentity.fromId("a"), location(2, 3), Some(location(6, 4)))
      }

      "should map resulting line & column numbers back to Chrome space" in {
        val debugger = setup
        val start = Location("a", 1, Some(0))
        val end = Location("a", 5, Some(0))

        val result = requestAndReceiveResponse(debugger, "2", Debugger.getPossibleBreakpoints(start, Some(end))) match {
          case result: Debugger.GetPossibleBreakpointsResult => result
          case other => fail("Unexpected: " + other)
        }

        result.locations should be (Seq(BreakLocation("a", 0, Some(0)), BreakLocation("a", 1, Some(0)), BreakLocation("a", 2, Some(0))))
      }
    }

    "ScriptAdded handling" - {
      def simulateScriptAdded(script: Script): Seq[Messages.Event] = {
        val debugger = newActorInstance[Debugger]

        // Receive at most 2 events.
        receiveScriptEventTriggeredEvents(debugger, Seq(Messages.Request("1", Domain.enable)),
          Seq(ScriptAdded(script)), 2)
      }
      def getEventParams(ev: Messages.Event): ScriptParsedEventParams = ev.params match {
        case p: ScriptParsedEventParams => p
        case other => throw new IllegalArgumentException("Unknown params: " + other)
      }

      "results in a ScriptParsed event for a new script" in {
        val events = simulateScriptAdded(script("xx1"))
        val scriptIds = events.map(getEventParams).map(_.scriptId)
        scriptIds should contain ("xx1")
      }

      "results in no event for an existing script" in {
        addScript(script("xx1"))
        val events = simulateScriptAdded(script("xx1"))
        val scriptIds = events.map(getEventParams).map(_.scriptId)
        // Only expect the event for the known script!
        scriptIds should be (Seq("xx1"))
      }

      "results in a ScriptParsed event for an existing script if the contents hash is new" in {
        addScript(script("xx1", hash = "hash1"))
        val events = simulateScriptAdded(script("xx1", hash = "hash2"))
        val scriptIds = events.map(getEventParams).map(_.scriptId)
        scriptIds should be (Seq("xx1", "xx1"))
      }
    }

    "ResolvedBreakpoint handling" - {
      def simulateBreakpointResolved(breakpointId: String, location: LocationInScript): Messages.Event = {
        val debugger = newActorInstance[Debugger]

        receiveScriptEventTriggeredEvent(debugger, Seq(Messages.Request("1", Domain.enable)),
          Seq(BreakpointResolved(breakpointId, location))
        )
      }

      def getEventParams(ev: Messages.Event): BreakpointResolvedEventParams = ev.params match {
        case p: BreakpointResolvedEventParams => p
        case other => throw new IllegalArgumentException("Unknown params: " + other)
      }

      "should result in a Debugger.breakpointResolved event" in {
        val ev = simulateBreakpointResolved("a", LocationInScript("s1", ScriptLocation(4, Some(1))))
        ev.method should be("Debugger.breakpointResolved")
      }

      "translates BreakpointResolved into event parameters" in {
        val ev = simulateBreakpointResolved("a", LocationInScript("s1", ScriptLocation(4, Some(1))))
        val eventParams = getEventParams(ev)
        eventParams should be (BreakpointResolvedEventParams("a", Debugger.Location("s1", 3, Some(0))))
      }
    }

    "HitBreakpoint handling" - {
      val aScriptURL = ScriptURL.create("http://programmaticallyspeaking.com/test.js")

      def simulateHitBreakpoint(stackFrames: Seq[StackFrame], reason: BreakpointReason = BreakpointReason.Breakpoint): Messages.Event = {
        val debugger = newActorInstance[Debugger]

        receiveScriptEventTriggeredEvent(debugger, Seq(Messages.Request("1", Domain.enable)),
          Seq(HitBreakpoint(stackFrames, Some("dummy"), reason))
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
          val thisObj = objectWithId("$$this")
          val scopeObjectId = ObjectId("$$scope")
          val scopeObj = ObjectNode("Object", scopeObjectId)
          val stackFrame = createStackFrame("sf1", thisObj, Some(Scope(scopeObj, scopeType)), "a", aScriptURL, location(10), "fun")
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)

          params.callFrames.head.scopeChain.find(_.`object`.objectId.contains(scopeObjectId.toString)) match {
            case Some(result) =>
              result.`type` should be (expected)
            case None => fail("Didn't find the scope")
          }

        }
      }

      "translates" - {
        val thisObj = objectWithId("$$this")
        val stackFrame = createStackFrame("sf1", thisObj, None, "a", aScriptURL, location(10), "fun")

        "Exception reason" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame), BreakpointReason.Exception(None))
          val params = getEventParams(ev)
          params.reason should be ("exception")
        }

        "Breakpoint reason" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.reason should be ("other")
        }

        "reason without data into no event params data" in {
          val ev = simulateHitBreakpoint(Seq(stackFrame))
          val params = getEventParams(ev)
          params.data should be ('empty)
        }

        "reason with data into reference data" in {
          val objId = ObjectId("err-id")
          val errValue = ErrorValue(ExceptionData("Error", "err", 1, 0, "", None), true, objId, None)
          val reason = BreakpointReason.Exception(Some(errValue))
          val ev = simulateHitBreakpoint(Seq(stackFrame), reason)
          val params = getEventParams(ev)
          params.data should be (Some(RemoteObject.forError("Error", "err", None, objId.toString)))
        }
      }

      "with a scope object" - {
        val thisObj = objectWithId("$$this")
        val scopeObj = objectWithId("$$scope")
        val stackFrame = createStackFrame("sf1", thisObj, Some(Scope(scopeObj, ScopeType.Closure)), "a", aScriptURL, location(10), "fun")

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
        val thisObj = objectWithId("$$this")
        val stackFrame = createStackFrame("sf1", thisObj, None, "a", aScriptURL, location(10), "fun")

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

      def testEvalHandling(theAnswer: => ValueNode, returnByValue: Option[Boolean] = None, generatePreview: Option[Boolean] = None)(fun: (Debugger.EvaluateOnCallFrameResult) => Unit): Unit = {
        when(currentScriptHost.evaluateOnStackFrame(any[String], any[String], any[Map[String, ObjectId]])).thenAnswer(new Answer[Try[ValueNode]] {
          override def answer(invocation: InvocationOnMock): Try[ValueNode] = {
            Try(theAnswer)
          }
        })

        val debugger = newActorInstance[Debugger]
        debugger ! Messages.Request("1", Domain.enable)

        inside(requestAndReceiveResponse(debugger, "", Debugger.evaluateOnCallFrame("a", "5+5", returnByValue, generatePreview))) {
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
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, isThrown = true, ObjectId("$$err"), None)) { resp =>
          resp.result should be (RemoteObject.undefinedValue)
        }
      }

      "should return a response with an error value when ScriptHost evaluation returns a non-Throwable-based error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, isThrown = false, ObjectId("$$err"), None)) { resp =>
          resp.result should be (RemoteObject.forError("Error", "oops", None, """{"id":"$$err"}"""))
        }
      }

      "should return a response with exception details when ScriptHost evaluation returns a Throwable-based error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, isThrown = true, ObjectId("$$err"), None)) { resp =>
          // Remember, Chrome line numbers are 0-based, so 10 => 9
          val exc = RemoteObject.forError("Error", "oops", Some("Error: oops"), """{"id":"$$err"}""")
          resp.exceptionDetails should be (Some(ExceptionDetails(1, "Uncaught", 9, 1, Some("<eval>"), None, Some(exc), Runtime.StaticExecutionContextId)))
        }
      }

      lazy val failException = {
        var ret: EvaluateOnCallFrameResult = null
        testEvalHandling(DebuggerTestHelper.fail("fail")) { resp =>
          ret = resp
        }
        ret
      }

      "should return a response with value 'undefined' when ScriptHost evaluation actually throws" in {
        failException.result should be (RemoteObject.undefinedValue)
      }

      "should convert an actual exception into exception details with text 'Uncaught' to mimic Chrome" in {
        failException.exceptionDetails.map(_.text) should be (Some("Uncaught"))
      }

      "should convert an actual exception into exception details with description" in {
        val desc = failException.exceptionDetails.flatMap(_.exception.flatMap(_.description)).getOrElse("")
        desc should include ("fail")
      }

      "should convert an actual exception into exception details with line number" in {
        // DebuggerTestHelper throws at line 6, which converted to base 0 becomes 5
        failException.exceptionDetails.map(_.lineNumber) should be (Some(5))
      }

      "should convert an actual exception into exception details with URL" in {
        inside(failException.exceptionDetails.flatMap(_.url)) {
          case Some(url) =>
            url should fullyMatch regex ("file:/.*DebuggerTestHelper\\.scala".r)
        }
      }

      "should support by-value return" in {
        val obj = objectWithId("x")
        setProperties(obj, Map("foo" -> "bar"))
        testEvalHandling(obj, returnByValue = Some(true)) { resp =>
          resp.result should be (RemoteObject.forObject(Map("foo" -> "bar")))
        }
      }

      "should generate a preview if requested" in {
        val obj = objectWithId("x")
        setProperties(obj, Map.empty)
        testEvalHandling(obj, generatePreview = Some(true)) { resp =>
          resp.result.preview should be ('defined)
        }
      }

      "should request own object properties and not only accessors when generating a preview" in {
        val obj = objectWithId("x")
        setProperties(obj, Map.empty)
        testEvalHandling(obj, generatePreview = Some(true)) { resp =>
          verify(currentScriptHost).getObjectProperties(ObjectId("x"), true, false)
        }
      }
    }

    "setSkipAllPauses" - {
      "should call the corresponding method on ScriptHost" in {
        val debugger = newActorInstance[Debugger]
        sendAndReceive(debugger, Messages.Request("1", Domain enable))
        sendAndReceive(debugger, Messages.Request("2", Debugger setSkipAllPauses true))

        verify(currentScriptHost).setSkipAllPauses(true)
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

    when(host.setBreakpoint(any[ScriptIdentity], any[ScriptLocation], any[Option[String]])).thenAnswerWith({
      case (scriptId: ScriptIdentity) :: (location: ScriptLocation) :: _ :: Nil =>
        val bpId = "bp_" + location.lineNumber1Based
        // Arbitrary test stuff. High line numbers don't exist!
        if (location.lineNumber1Based > 100) Breakpoint(bpId, Seq.empty)
        else {
          activeBreakpoints += bpId
          val sid = scriptId match {
            case IdBasedScriptIdentity(x) => x
            case URLBasedScriptIdentity(url) => url
            case URLRegexBasedScriptIdentity(re) => "re_" + re
            case HashBasedScriptIdentity(h) => "h_" + h
          }
          val theScriptId = sid + "_id"
          Breakpoint(bpId, Seq(LocationInScript(theScriptId, location)))
        }
    })
    when(host.removeBreakpointById(any[String])).thenAnswerWith({
      case (id: String) :: Nil =>
        activeBreakpoints -= id
    })
    when(host.getObjectProperties(any[ObjectId], any[Boolean], any[Boolean])).thenAnswer((invocation: InvocationOnMock) => {
      val objectId = invocation.getArgument[ObjectId](0)
      objectProperties.get(objectId) match {
        case Some(props) =>
          props.map { e =>
            e._1 -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, true, true, true, Some(SimpleValue(e._2)), None, None)
          }.toSeq

        case None => throw new IllegalArgumentException("Unknown object ID: " + objectId)
      }
    })

    host
  }

  def createStackFrame(Aid: String, AthisObj: ValueNode, scope: Option[Scope], aScriptId: String, aScriptURL: ScriptURL, aLocation: ScriptLocation, functionName: String) = new StackFrame {
    override val thisObj: ValueNode = AthisObj
    override val scopeChain = scope.toSeq
    override val id: String = Aid
    override val functionDetails: FunctionDetails = FunctionDetails(functionName)
    override val scriptId: String = aScriptId
    override val scriptURL: ScriptURL = aScriptURL
    override val location: ScriptLocation = aLocation
  }
}

class DebuggerObjectTest extends UnitTest {

  def originalScript(source: String, path: String, id: String) = ScriptImpl.fromSource(ScriptURL.create(path), source, id)

  def fakeFileReader(contents: Map[File, String]) = new FileReader {
    override def read(file: File, charset: Charset): Try[String] = {
      contents.get(file) match {
        case Some(src) => Success(src)
        case None => Failure(new FileNotFoundException(file.getAbsolutePath))
      }
    }
  }

  "scriptWithPublishedFiles" - {
    "with a script compiled with source map support" - {
      val coffee = "->"
      val js =
        """// Generated by CoffeeScript 1.12.4
          |(function() {
          |  (function() {});
          |
          |}).call(this);
          |
          |//# sourceMappingURL=file.js.map
        """.stripMargin
      val map =
        """{
          |  "version": 3,
          |  "file": "file.js",
          |  "sourceRoot": "",
          |  "sources": [
          |    "file.coffee"
          |  ],
          |  "names": [],
          |  "mappings": ";AAAA;EAAA,CAAA,SAAA,GAAA,CAAA;AAAA"
          |}
        """.stripMargin
      val coffeeFile = new File("/c:/path/to/file.coffee")
      val mapFile = new File("/c:/path/to/file.js.map")
      val jsFile = new File("/c:/path/to/file.js")
      implicit val reader = fakeFileReader(Map(
        coffeeFile -> coffee,
        mapFile -> map,
        jsFile -> js
      ))

      val publisher = new FakePublisher
      val script = originalScript(js, "/c:/path/to/file.js", "a")
      def newScript = Debugger.scriptWithPublishedFiles(script, publisher)

      "publishes the map file" in {
        newScript
        publisher.published should contain (mapFile)
      }

      "publishes the coffee file" in {
        newScript
        publisher.published should contain (coffeeFile)
      }

      "doesn't publish the JS file since it's not necessary (the source is present)" in {
        newScript
        publisher.published should not contain (jsFile)
      }

      "rewrites the source mapping URL" in {
        newScript.sourceMapUrl().map(_.toString) should be (publisher.urlFor(mapFile))
      }
    }

  }

  class FakePublisher extends FilePublisher {

    val _published = mutable.Map[File, URL]()

    def published = _published.keys.toSeq
    def urlFor(f: File): Option[String] = _published.get(f).map(_.toString)

    override def publish(file: File): URL = {
      val url = new URL("http", "localhost", 8080, file.toURI.toURL.toString.replace("file:", ""))
      _published(file) = url
      url
    }
  }
}