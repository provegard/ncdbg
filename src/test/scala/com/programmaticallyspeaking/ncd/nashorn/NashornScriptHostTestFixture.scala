package com.programmaticallyspeaking.ncd.nashorn

import java.io._
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.ConcurrentLinkedQueue

import ch.qos.logback.classic.Level
import com.programmaticallyspeaking.ncd.host.{ExceptionPauseType, ScriptEvent}
import com.programmaticallyspeaking.ncd.messaging.{Observer, SerializedSubject, Subscription}
import com.programmaticallyspeaking.ncd.testing.{MemoryAppender, SharedInstanceActorTesting, StringUtils, UnitTest}
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
import scala.util.{Failure, Success, Try}
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

object Signals {
  val go = "go"
  val ready = "ready"
  val scriptDone = "script_done"
}

trait VirtualMachineLauncher { self: SharedInstanceActorTesting with Logging =>
  import scala.collection.JavaConverters._

  val scriptExecutor: ScriptExecutorBase

  def logVirtualMachineOutput(output: String) = {
    // When we receive "ready", the VM is ready to listen for "go".
    if (output == Signals.ready) {
      log.info("Got the ready signal from the VM")
      vmReadyPromise.success(())
    } else if (output == Signals.scriptDone) {
      vmScriptDonePromise.success(())
    } else {
      log.info("VM output: " + output)
    }
  }
  def logVirtualMachineError(error: String) = {
    log.error("VM error: " + error)
  }

  implicit val executionContext: ExecutionContext

  val resultTimeout: FiniteDuration = 12.seconds
  val scriptDoneTimeout: FiniteDuration = 5.seconds

  private var vm: VirtualMachine = _
  private var host: NashornScriptHost = _

  private var vmStdinWriter: PrintWriter = _
  protected var vmRunningPromise: Promise[NashornScriptHost] = _
  protected var vmReadyPromise: Promise[Unit] = _
  protected var vmScriptDonePromise: Promise[Unit] = _

  // Tracks progress for better timeout failure reporting
  private val progress = new ConcurrentLinkedQueue[String]()

  private var logSubscription: Subscription = _
  private var hostEventSubscription: Subscription = _

  protected val eventSubject = new SerializedSubject[ScriptEvent]

  private def nowString = ZonedDateTime.now().format(DateTimeFormatter.ISO_INSTANT)
  protected def reportProgress(msg: String): Unit = progress.add(s"[$nowString] $msg")

  protected def summarizeProgress() = progress.asScala.mkString("\n")
  protected def clearProgress() = progress.clear()

  private def start(): Unit = {
    vm = launchVm()
    vmStdinWriter = new PrintWriter(new OutputStreamWriter(vm.process().getOutputStream()), true)
    val debugger = new NashornDebugger()
    host = debugger.create(vm)

    vmRunningPromise = Promise[NashornScriptHost]()
    vmReadyPromise = Promise[Unit]()

    logSubscription = MemoryAppender.logEvents.subscribe(Observer.from {
      case event if event.getLevel.isGreaterOrEqual(Level.DEBUG) => // if event.getLoggerName == getClass.getName =>
        val simpleLoggerName = event.getLoggerName.split('.').last
        var txt = s"[$simpleLoggerName][${event.getLevel}]: ${event.getMessage}"
        Option(event.getThrowableProxy).foreach { proxy =>
          txt += "\n" + proxy.getMessage
          proxy.getStackTraceElementProxyArray.foreach { st =>
            txt += "\n  " + st.toString
          }
        }
        reportProgress(txt)
    })

    setupHost()
  }

  private def stop(): Unit = {
    clearProgress()
    Option(vm).foreach(_.process().destroy())
    Option(logSubscription).foreach(_.unsubscribe())
    Option(vmStdinWriter).foreach(_.close())
    Option(hostEventSubscription).foreach(_.unsubscribe())
    vm = null
    logSubscription = null
    vmStdinWriter = null
    hostEventSubscription = null
  }

  protected def restart(): Unit = {
    stop()
    start()
  }

  override def beforeAllTests(): Unit = start()

  protected def sendToVm(data: String, encodeBase64: Boolean = false): Unit = {
    val dataToSend = if (encodeBase64) StringUtils.toBase64(data) else data
    log.info("Sending to VM: " + dataToSend)
    vmStdinWriter.println(dataToSend)
  }

  protected def setupHost(): Unit = {
    log.info("VM is running, setting up host")
    // Capture to prevent completing an old promise during restart.
    val capturedRunningPromise = vmRunningPromise
    hostEventSubscription = host.events.subscribe(new Observer[ScriptEvent] {
      override def onError(error: Throwable): Unit = capturedRunningPromise.tryFailure(error)

      override def onComplete(): Unit = capturedRunningPromise.tryFailure(new Exception("complete"))

      override def onNext(item: ScriptEvent): Unit = item match {
        case InitialInitializationComplete =>
          // Host initialization is complete, so let ScriptExecutor know that it can continue.
          log.info("host initialization complete")

          // Wait until we observe that the VM is ready to receive the go command.
          vmReadyPromise.future.onComplete {
            case Success(_) =>
              log.info("VM is ready!")

              sendToVm(Signals.go)

              // Resolve the promise on which we chain script execution in runScript. This means that any script execution
              // will wait until the infrastructure is ready.
              capturedRunningPromise.trySuccess(host)

            case Failure(t) => capturedRunningPromise.tryFailure(t)
          }
        case other =>
          log.debug("Dispatching to event observers: " + other)
          eventSubject.onNext(other)
      }
    })
    host.pauseOnBreakpoints()
  }

  override def afterAllTests(): Unit = stop()

  protected def getHost = Option(host).getOrElse(throw new IllegalStateException("Host not set"))

  private def launchVm(): VirtualMachine = {
    val conn = findLaunchingConnector()
    val args = conn.defaultArguments()

    val cp = System.getProperty("java.class.path")
    val className = scriptExecutor.getClass.getName.replaceAll("\\$$", "")
    val mainArg = args.get("main")
    mainArg.setValue(s"""-Dnashorn.args=--language=es6 -cp "$cp" $className""")

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

trait NashornScriptHostTestFixture extends UnitTest with Logging with SharedInstanceActorTesting with VirtualMachineLauncher {
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

  protected def observeAndRunScriptAsync[R](script: String, observer: Observer[ScriptEvent] = null)(handler: (NashornScriptHost) => Future[R]): Unit = {
    Option(observer).foreach(addObserver)

    // Wait separately for the VM to run. Otherwise, a slow-started VM may "eat up" the test timeout.
    val host = try Await.result(vmRunningPromise.future, resultTimeout) catch {
      case _: TimeoutException => throw new TimeoutException("Timed out waiting for the VM to start running. Progress:\n" + summarizeProgress())
    }

    // This promise is resolved when we observe that the VM is done with script execution.
    vmScriptDonePromise = Promise[Unit]()

    clearProgress() // New progress for each test.
    log.info(">>>>>> TEST START")
    log.info("VM running, sending script")
    sendToVm(script, encodeBase64 = true)

    // Reset things
    host.setSkipAllPauses(false)
    host.pauseOnExceptions(ExceptionPauseType.None)

    var thrownEx: Throwable = null
    try Await.result(handler(host), resultTimeout) catch {
      case _: TimeoutException =>
        thrownEx = new TimeoutException(s"No results within ${resultTimeout.toMillis} ms. Progress:\n" + summarizeProgress())
        throw thrownEx
      case NonFatal(t) =>
        thrownEx = t
        throw t
    } finally {
      // getHost may throw if the host isn't set
      Try(getHost.resume())

      try Await.result(vmScriptDonePromise.future, scriptDoneTimeout) catch {
        case _: TimeoutException =>
          val toThrow = new TimeoutException("VM script execution didn't finish")
          // If we have an exception above, don't hide it
          Option(thrownEx) match {
            case Some(e) => e.addSuppressed(toThrow)
            case None => throw toThrow
          }
      }
    }
  }

  protected def observeAndRunScriptSync[R](script: String, observer: Observer[ScriptEvent])(handler: (NashornScriptHost) => R): Unit = {
    observeAndRunScriptAsync(script, observer) { host => Future { handler(host) }}
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
  println(Signals.ready)
  waitForSignal(Signals.go)
  println("Got the go signal!")

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
    println(Signals.scriptDone)
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
      case _: InterruptedException =>
      // ok
      case ex: IOException =>
        if (ex.getMessage != "Stream closed") ex.printStackTrace(System.err)
      case NonFatal(t) =>
        t.printStackTrace(System.err)
    }
  }
}