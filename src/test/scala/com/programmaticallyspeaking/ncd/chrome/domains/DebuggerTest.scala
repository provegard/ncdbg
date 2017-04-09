package com.programmaticallyspeaking.ncd.chrome.domains

import java.io.{File, FileNotFoundException}
import java.net.URL
import java.nio.charset.Charset

import akka.actor.{ActorRef, PoisonPill}
import akka.testkit.TestProbe
import com.programmaticallyspeaking.ncd.chrome.domains.Debugger.{Location, PausedEventParams, ScriptParsedEventParams}
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
  import org.mockito.ArgumentMatchers._
  import com.programmaticallyspeaking.ncd.testing.MockingUtils._

  implicit val container = new Container(Seq(FakeFilePublisher))

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

  def location(ln: Int) = ScriptLocation(ln, 1)
  def location(ln: Int, cn: Int) = ScriptLocation(ln, cn)

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
      //TODO: assert event details

      "should tell the host to pause on breakpoints" in {
        val debugger = newActorInstance[Debugger]

        requestAndReceiveResponse(debugger, "1", Domain.enable)

        verify(currentScriptHost).pauseOnBreakpoints()
      }
    }

    "setPauseOnExceptions" - {
      forAll(setPauseOnExceptionsCases) { (state, expected) =>
        s"should handle state '$state'" in {
          val debugger = newActorInstance[Debugger]
          requestAndReceiveResponse(debugger, "1", Domain.enable)

          // Clear invocations since Domain.enable generates a call to setBreakpointsActive also
//          clearInvocations(currentScriptHost)

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

      "should return null breakpoint ID if the breakpoint location is unknown" in {
        val result = setBreakpointByUrl(101, "a")
        result.breakpointId should be (null)
      }

      "should return no locatoins if the breakpoint location is unknown" in {
        val result = setBreakpointByUrl(101, "a")
        result.locations should be ('empty)
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

    "getPossibleBreakpoints" - {

      def setup: ActorRef = {
        when(currentScriptHost.getBreakpointLocations(any[String], any[ScriptLocation], any[Option[ScriptLocation]]))
          .thenReturn(Seq(location(1), location(2), location(3)))

        val debugger = newActorInstance[Debugger]
        debugger ! Messages.Request("1", Domain.enable)
        debugger
      }

      "should invoke ScriptHost, converting Chrome line/column numbers (0-based) to Nashorn line/column numbers (1-based)" in {
        val debugger = setup
        val start = Location("a", 1, 2)
        val end = Location("a", 5, 3)

        requestAndReceiveResponse(debugger, "2", Debugger.getPossibleBreakpoints(start, Some(end)))

        verify(currentScriptHost).getBreakpointLocations("a", location(2, 3), Some(location(6, 4)))
      }

      "should map resulting line numbers back to Chrome space" in {
        val debugger = setup
        val start = Location("a", 1, 0)
        val end = Location("a", 5, 0)

        val result = requestAndReceiveResponse(debugger, "2", Debugger.getPossibleBreakpoints(start, Some(end))) match {
          case result: Debugger.GetPossibleBreakpointsResult => result
          case other => fail("Unexpected: " + other)
        }

        result.locations should be (Seq(Location("a", 0, 0), Location("a", 1, 0), Location("a", 2, 0)))
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
          val thisObj = objectWithId("$$this")
          val scopeObjectId = ObjectId("$$scope")
          val scopeObj = ObjectNode("Object", scopeObjectId)
          val stackFrame = createStackFrame("sf1", thisObj, Some(Scope(scopeObj, scopeType)), Breakpoint("bp1", "a", location(10)), "fun")
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
        val thisObj = objectWithId("$$this")
        val scopeObj = objectWithId("$$scope")
        val stackFrame = createStackFrame("sf1", thisObj, Some(Scope(scopeObj, ScopeType.Closure)), Breakpoint("bp1", "a", location(10)), "fun")

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
        val stackFrame = createStackFrame("sf1", thisObj, None, Breakpoint("bp1", "a", location(10)), "fun")

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
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, isThrown = true, ObjectId("$$err"))) { resp =>
          resp.result should be (RemoteObject.undefinedValue)
        }
      }

      "should return a response with an error value when ScriptHost evaluation returns a non-Throwable-based error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, isThrown = false, ObjectId("$$err"))) { resp =>
          resp.result should be (RemoteObject.forError("Error", "oops", None, """{"id":"$$err"}"""))
        }
      }

      "should return a response with exception details when ScriptHost evaluation returns a Throwable-based error value" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, isThrown = true, ObjectId("$$err"))) { resp =>
          // Remember, Chrome line numbers are 0-based, so 10 => 9
          resp.exceptionDetails should be (Some(ExceptionDetails(1, "oops", 9, 1, Some("<eval>"), None, Runtime.StaticExecutionContextId)))
        }
      }

      "should return a response with _only_ value 'undefined' when ScriptHost evaluation returns a Throwable-based error value but silent mode is requested" in {
        val exData = ExceptionData("Error", "oops", 10, 1, "<eval>", None)
        testEvalHandling(ErrorValue(exData, isThrown = true, ObjectId("$$err")), Some(true)) { resp =>
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

    when(host.setBreakpoint(any[String], any[ScriptLocation])).thenAnswerWith({
      case (uri: String) :: (scriptLoc : ScriptLocation) :: Nil =>
        val id = "bp_" + scriptLoc.lineNumber1Based
        // Arbitrary test stuff. High line numbers don't exist!
        if (scriptLoc.lineNumber1Based > 100) None
        else {
          activeBreakpoints += id
          Some(Breakpoint(id, uri + "_id", scriptLoc))
        }
    })
    when(host.removeBreakpointById(any[String])).thenAnswerWith({
      case (id: String) :: Nil =>
        activeBreakpoints -= id
        Done
    })
    when(host.getObjectProperties(any[ObjectId], any[Boolean], any[Boolean])).thenAnswer((invocation: InvocationOnMock) => {
      val objectId = invocation.getArgument[ObjectId](0)
      objectProperties.get(objectId) match {
        case Some(props) =>
          props.map { e =>
            e._1 -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, false, true, true, true, Some(SimpleValue(e._2)), None, None)
          }

        case None => throw new IllegalArgumentException("Unknown object ID: " + objectId)
      }
    })

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

class DebuggerObjectTest extends UnitTest {

  def originalScript(source: String, path: String, id: String) = ScriptImpl.fromSource(path, source, id)

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