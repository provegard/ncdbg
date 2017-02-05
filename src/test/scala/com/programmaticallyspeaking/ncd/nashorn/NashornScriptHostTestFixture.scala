package com.programmaticallyspeaking.ncd.nashorn

import java.io._
import java.util

import com.programmaticallyspeaking.ncd.host.ScriptEvent
import com.programmaticallyspeaking.ncd.messaging.{Observer, SerializedSubject, Subscription}
import com.programmaticallyspeaking.ncd.testing.{FreeActorTesting, StringUtils, UnitTest}
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.VMStartEvent
import com.sun.jdi.{Bootstrap, VirtualMachine}
import jdk.nashorn.api.scripting.{AbstractJSObject, NashornScriptEngineFactory}
import org.slf4s.Logging

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.Try
import scala.util.control.NonFatal

trait NashornScriptHostTestFixture extends UnitTest with Logging with FreeActorTesting {
  import scala.collection.JavaConverters._

  def logVirtualMachineOutput(output: String) = log.info("VM output: " + output)
  def logVirtualMachineError(error: String) = log.error("VM error: " + error)

  implicit val executionContext: ExecutionContext

  val resultTimeout: FiniteDuration

  private var vm: VirtualMachine = _
  private var host: NashornScriptHost = _

  private var vmStdinWriter: PrintWriter = _
  private var vmRunningFuture: Promise[Unit] = _

  private val eventSubject = new SerializedSubject[ScriptEvent]
  private val subscriptions = mutable.Queue[Subscription]()

  override def beforeAllTests(): Unit = {
    vm = launchVm()
    vmStdinWriter = new PrintWriter(new OutputStreamWriter(vm.process().getOutputStream()), true)
    val debugger = new NashornDebugger()
    host = debugger.create(vm)

    vmRunningFuture = Promise[Unit]()

    setupHost()
  }

  private def sendToVm(data: String, encodeBase64: Boolean = false): Unit = {
    val dataToSend = if (encodeBase64) StringUtils.toBase64(data) else data
    vmStdinWriter.println(dataToSend)
  }

  protected def setupHost(): Unit = {
    host.events.subscribe(new Observer[ScriptEvent] {
      override def onError(error: Throwable): Unit = vmRunningFuture.tryFailure(error)

      override def onComplete(): Unit = vmRunningFuture.tryFailure(new Exception("complete"))

      override def onNext(item: ScriptEvent): Unit = item match {
        case InitialInitializationComplete =>
          // Host initialization is complete, so let ScriptExecutor know that it can continue.
          sendToVm("go")

          // Resolve the promise on which we chain script execution in runScript. This means that any script execution
          // will wait until the infrastructure is ready.
          vmRunningFuture.trySuccess(())
        case other => eventSubject.onNext(item)
      }
    })
    host.pauseOnBreakpoints()
  }

  override def afterAllTests(): Unit = {
    vm.process().destroy()
  }

  private def addObserver(observer: Observer[ScriptEvent]): Unit = {
    while (subscriptions.nonEmpty) {
      subscriptions.dequeue().unsubscribe()
    }

    subscriptions.enqueue(eventSubject.subscribe(observer))
  }

  protected def getHost = {
    if (host == null) throw new IllegalStateException("Host not set")
    host
  }

  protected def runScriptWithObserverSync[R](script: String, observer: Observer[ScriptEvent])(handler: (NashornScriptHost) => Future[R]): Unit = {
    addObserver(observer)

    val f = vmRunningFuture.future.flatMap { _ =>
      sendToVm(script, encodeBase64 = true)
      handler(host)
    }
    Await.result(f, resultTimeout)
  }

  private def launchVm(): VirtualMachine = {
    val conn = findLaunchingConnector()
    val args = conn.defaultArguments()

    val cp = System.getProperty("java.class.path")
    val className = ScriptExecutor.getClass.getName.replaceAll("\\$$", "")
    val mainArg = args.get("main")
    mainArg.setValue(s"""-cp "$cp" $className""")

    val vm = conn.launch(args)

    new StreamReadingThread(vm.process().getInputStream(), logVirtualMachineOutput).start()
    new StreamReadingThread(vm.process().getErrorStream(), logVirtualMachineError).start()

    waitUntilStarted(vm)
    vm
  }

  private def waitUntilStarted(vm: VirtualMachine): Unit = {
    var attempts = 5
    var done = false
    while (!done && attempts >= 0) {
      val eventSet = vm.eventQueue().remove(500L)
      Option(eventSet).foreach { es =>
        es.asScala.foreach {
          case ev: VMStartEvent =>
            done = true
          case _ =>
        }
        es.resume()
      }
      attempts -= 1
    }
    if (!done) throw new Exception("VM didn't start")
  }

  private def findLaunchingConnector(): LaunchingConnector = {

    Bootstrap.virtualMachineManager().defaultConnector()
    //    Bootstrap.virtualMachineManager().allConnectors().asScala.find(_.name() == "com.sun.jdi.CommandLineLaunch") match {
    //      case Some(c: LaunchingConnector) => c
    //      case _ => throw new IllegalStateException("Found no LaunchingConnector")
    //    }
  }
}


object ScriptExecutor extends App {
  println("ScriptExecutor starting. Java version: " + System.getProperty("java.version"))
  val scriptEngine = new NashornScriptEngineFactory().getScriptEngine
  val reader = new BufferedReader(new InputStreamReader(System.in))
  println("Awaiting go signal")
  val signal = readStdin()
  if (signal != "go") {
    println("Didn't get go signal, got: " + signal)
    System.exit(1)
  }

  val bindings = scriptEngine.createBindings()
  bindings.put("createArray", new FunctionLikeJSObject((args) => new ArrayLikeJSObject(args)))
  bindings.put("createObject", new FunctionLikeJSObject((args) => {
    // assume key,value,key,value...
    assert(args.size % 2 == 0, "must have even no. of args")
    val theMap = args.grouped(2).map(tup => tup(0).toString -> tup(1)).toMap
    new ObjectLikeJSObject(theMap)
  }))
  bindings.put("createFunctionThatReturns", new FunctionLikeJSObject((args) => new FunctionLikeJSObject(_ => args(0))))

  while (true) {
    println("Awaiting script on stdin...")
    val script = StringUtils.fromBase64(readStdin())
    println("Got script: " + script)
    try {
      scriptEngine.eval(script, bindings)
      println("Script evaluation completed without errors")
    } catch {
      case NonFatal(t) =>
        t.printStackTrace(System.err)
    }
  }

  private def readStdin(): String = reader.readLine()
}

class StreamReadingThread(in: InputStream, appender: (String) => Unit) extends Thread {
  override def run(): Unit = {
    try {
      val reader = new BufferedReader(new InputStreamReader(in))
      var str = ""
      while (str != null) {
        str = reader.readLine()
        Option(str).foreach(appender)
      }
    } catch {
      case ex: InterruptedException =>
      // ok
      case ex: IOException =>
        ex.printStackTrace(System.err)
    }
  }
}

class ArrayLikeJSObject(items: Seq[AnyRef]) extends AbstractJSObject {
  import scala.collection.JavaConverters._
  override def hasSlot(slot: Int): Boolean = slot >= 0 && slot < items.size

  override def getSlot(index: Int): AnyRef = items(index)

  override def hasMember(name: String): Boolean = Try(name.toInt).map(hasSlot).getOrElse(name == "length")

  override def getMember(name: String): AnyRef = Try(name.toInt).map(getSlot).getOrElse(if (name == "length") items.size.asInstanceOf[AnyRef] else null)

  override def getClassName: String = "Array"

  override def isArray: Boolean = true

  override def keySet(): util.Set[String] = (items.indices.map(_.toString) :+ "length").toSet.asJava

  override def values(): util.Collection[AnyRef] = items.asJava
}

class ObjectLikeJSObject(data: Map[String, AnyRef]) extends AbstractJSObject {
  import scala.collection.JavaConverters._

  override def values(): util.Collection[AnyRef] = data.values.toList.asJava

  override def hasMember(name: String): Boolean = data.contains(name)

  override def getMember(name: String): AnyRef = data(name)

  override def getClassName: String = "Object"

  override def keySet(): util.Set[String] = data.keySet.asJava
}

class FunctionLikeJSObject(fun: (Seq[AnyRef]) => AnyRef) extends AbstractJSObject {
  override def getClassName: String = "Function"

  override def call(thiz: scala.Any, args: AnyRef*): AnyRef = fun(args.toSeq)

  override def isFunction: Boolean = true
}