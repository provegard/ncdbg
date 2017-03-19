package com.programmaticallyspeaking.ncd.nashorn

import java.io._
import java.util.concurrent.ConcurrentLinkedQueue

import com.programmaticallyspeaking.ncd.host.ScriptEvent
import com.programmaticallyspeaking.ncd.messaging.{Observer, SerializedSubject, Subscription}
import com.programmaticallyspeaking.ncd.testing.{FreeActorTesting, StringUtils, UnitTest}
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.VMStartEvent
import com.sun.jdi.{Bootstrap, VirtualMachine}
import jdk.nashorn.api.scripting.NashornScriptEngineFactory
import org.scalatest.concurrent.{AbstractPatienceConfiguration, PatienceConfiguration}
import org.scalatest.time.{Millis, Seconds, Span}
import org.slf4s.Logging

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent._
import scala.util.control.NonFatal

/** Provides a patience configuration that has a timeout that is shorter than the default timeout in
  * [[VirtualMachineLauncher]]. This ensures that any ScalaTest wait operations complete/stop before our own.
  */
trait FairAmountOfPatience extends AbstractPatienceConfiguration { this: PatienceConfiguration =>

  private val defaultPatienceConfig: PatienceConfig =
    PatienceConfig(
      timeout = scaled(Span(10, Seconds)),
      interval = scaled(Span(150, Millis))
    )

  implicit abstract override val patienceConfig: PatienceConfig = defaultPatienceConfig
}

trait VirtualMachineLauncher { self: FreeActorTesting with Logging =>
  import scala.collection.JavaConverters._

  val scriptExecutor: ScriptExecutorBase

  def logVirtualMachineOutput(output: String) = {
    reportProgress("VM output: " + output)
    log.info("VM output: " + output)
  }
  def logVirtualMachineError(error: String) = {
    reportProgress("VM error: " + error)
    log.error("VM error: " + error)
  }

  implicit val executionContext: ExecutionContext

  val resultTimeout: FiniteDuration = 12.seconds

  private var vm: VirtualMachine = _
  private var host: NashornScriptHost = _

  private var vmStdinWriter: PrintWriter = _
  protected var vmRunningPromise: Promise[NashornScriptHost] = _

  // Tracks progress for better timeout failure reporting
  private val progress = new ConcurrentLinkedQueue[String]()

  protected val eventSubject = new SerializedSubject[ScriptEvent]

  protected def reportProgress(msg: String): Unit = progress.add(msg)

  protected def summarizeProgress() = progress.asScala.mkString("\n")

  override def beforeAllTests(): Unit = {
    vm = launchVm()
    vmStdinWriter = new PrintWriter(new OutputStreamWriter(vm.process().getOutputStream()), true)
    val debugger = new NashornDebugger()
    host = debugger.create(vm)

    vmRunningPromise = Promise[NashornScriptHost]()

    setupHost()
  }

  protected def sendToVm(data: String, encodeBase64: Boolean = false): Unit = {
    val dataToSend = if (encodeBase64) StringUtils.toBase64(data) else data
    vmStdinWriter.println(dataToSend)
  }

  protected def setupHost(): Unit = {
    host.events.subscribe(new Observer[ScriptEvent] {
      override def onError(error: Throwable): Unit = vmRunningPromise.tryFailure(error)

      override def onComplete(): Unit = vmRunningPromise.tryFailure(new Exception("complete"))

      override def onNext(item: ScriptEvent): Unit = item match {
        case InitialInitializationComplete =>
          reportProgress("host initialization complete")

          // Host initialization is complete, so let ScriptExecutor know that it can continue.
          sendToVm("go")

          // Resolve the promise on which we chain script execution in runScript. This means that any script execution
          // will wait until the infrastructure is ready.
          vmRunningPromise.trySuccess(host)
        case other => eventSubject.onNext(other)
      }
    })
    host.pauseOnBreakpoints()
  }

  override def afterAllTests(): Unit = vm.process().destroy()

  protected def getHost = Option(host).getOrElse(throw new IllegalStateException("Host not set"))

  private def launchVm(): VirtualMachine = {
    val conn = findLaunchingConnector()
    val args = conn.defaultArguments()

    val cp = System.getProperty("java.class.path")
    val className = scriptExecutor.getClass.getName.replaceAll("\\$$", "")
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
          case _: VMStartEvent =>
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

trait NashornScriptHostTestFixture extends UnitTest with Logging with FreeActorTesting with VirtualMachineLauncher {
  implicit val executionContext: ExecutionContext

  override val scriptExecutor = ScriptExecutor

  private val subscriptions = mutable.Queue[Subscription]()
  private def unsubscribeAll(): Unit =
    while (subscriptions.nonEmpty) {
      subscriptions.dequeue().unsubscribe()
    }

  override def afterAllTests(): Unit = try unsubscribeAll() finally super.afterAllTests()

  protected def addObserver(observer: Observer[ScriptEvent]): Unit = {
    unsubscribeAll()
    subscriptions.enqueue(eventSubject.subscribe(observer))
  }

  protected def observeAndRunScriptAsync[R](script: String, observer: Observer[ScriptEvent])(handler: (NashornScriptHost) => Future[R]): Unit = {
    addObserver(observer)

    val f = vmRunningPromise.future.flatMap { host =>
      reportProgress("VM running, sending script")
      sendToVm(script, encodeBase64 = true)
      handler(host)
    }
    try Await.result(f, resultTimeout) catch {
      case t: TimeoutException =>
        throw new TimeoutException(s"No results with ${resultTimeout.toMillis} ms. Progress:\n" + summarizeProgress())
    }
  }

  protected def observeAndRunScriptSync[R](script: String, observer: Observer[ScriptEvent])(handler: (NashornScriptHost) => R): Unit = {
    addObserver(observer)

    val f = vmRunningPromise.future.map { host =>
      sendToVm(script, encodeBase64 = true)
      handler(host)
    }
    Await.result(f, resultTimeout)
  }
}

trait ScriptExecutorBase {
  val reader: BufferedReader

  protected def readStdin(): String = reader.readLine()
  protected def waitForSignal(expected: String): Unit = {
    println(s"Awaiting '$expected' signal")
    val signal = readStdin()
    if (signal != expected) {
      println(s"Didn't get '$expected' signal, got: " + signal)
      System.exit(1)
    }
  }
}

object ScriptExecutor extends App with ScriptExecutorBase {
  println("ScriptExecutor starting. Java version: " + System.getProperty("java.version"))
  val scriptEngine = new NashornScriptEngineFactory().getScriptEngine
  val reader = new BufferedReader(new InputStreamReader(System.in))
  waitForSignal("go")

  scriptEngine.eval(
    """this.createInstance = function (typeName) {
      |  var Type = Java.type(typeName);
      |  if (!Type) throw new Error("No such type: " + typeName);
      |  return new Type();
      |};
    """.stripMargin)

  while (true) {
    println("Awaiting script on stdin...")
    val script = StringUtils.fromBase64(readStdin())
    println("Got script: " + script)
    try {
      scriptEngine.eval(script)
      println("Script evaluation completed without errors")
    } catch {
      case NonFatal(t) =>
        t.printStackTrace(System.err)
    }
  }
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