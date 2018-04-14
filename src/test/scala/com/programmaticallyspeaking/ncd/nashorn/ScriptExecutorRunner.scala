package com.programmaticallyspeaking.ncd.nashorn

import java.io.{OutputStreamWriter, PrintWriter}
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.TimeoutException

import akka.actor.{Actor, ActorRef}
import com.programmaticallyspeaking.ncd.host.{InitialInitializationComplete, ScriptEvent}
import com.programmaticallyspeaking.ncd.infra.{CancellableFuture, DelayedFuture}
import com.programmaticallyspeaking.ncd.messaging.{Observer, SerializedSubject, Subscription}
import com.programmaticallyspeaking.ncd.nashorn.ScriptExecutorRunner.ScriptFailure
import com.programmaticallyspeaking.ncd.testing.{MemoryAppender, StringUtils}
import com.sun.jdi.{Bootstrap, VirtualMachine}
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.VMStartEvent
import org.slf4s.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object ScriptExecutorRunner {
  case class Start(javaHome: Option[String], extraArgs: Seq[String], timeout: FiniteDuration)
  case class Started(host: NashornScriptHost)
  case class StartError(progress: String, error: Option[Throwable])

  case class ExecuteScript(script: String, observer: Observer[ScriptEvent], timeout: FiniteDuration)
  case object ScriptExecutionDone
  case class ScriptFailure(reason: String)
  case object ScriptWillExecute

  case class ObserveScriptEvents(observer: Observer[ScriptEvent])
  case class ObserveScriptEventsResponse(subscription: Subscription)

  case object GetProgress
  case class ProgressResponse(progress: String)

  case object Stop

  private case class StdOut(text: String)
  private case class StdErr(text: String)
  private case class HostFailure(t: Throwable)
  private case object HostInitComplete
  private case object VmIsReady
  private case class ReportProgress(p: String)
}

class ScriptExecutorRunner(scriptExecutor: ScriptExecutorBase)(implicit executionContext: ExecutionContext) extends Actor with Logging {
  import scala.collection.JavaConverters._
  import ScriptExecutorRunner._

  private var vmStdinWriter: PrintWriter = _
  private var hostEventSubscription: Subscription = _
  private var host: NashornScriptHost = _
  private var virtualMachine: VirtualMachine = _

  // Tracks progress for better timeout failure reporting
  private val progress = ListBuffer[String]()
  private def nowString = ZonedDateTime.now().format(DateTimeFormatter.ISO_INSTANT)
  private def reportProgress(msg: String): Unit = progress += s"[$nowString] $msg"
  private def summarizeProgress() = progress.mkString("\n")
  private def clearProgress() = progress.clear()

  private var startSender: ActorRef = _
  private var scriptSender: ActorRef = _
  private var scriptTimeoutHandler: TimeoutHandler = _
  private var startTimeoutFuture: CancellableFuture[Unit] = _
  private var startTimeout: FiniteDuration = _

  private var logSubscription: Subscription = _

  private val eventSubject = new SerializedSubject[ScriptEvent]
  private val subscriptions = mutable.Queue[Subscription]()

  private def unsubscribeAll(): Unit = while (subscriptions.nonEmpty) subscriptions.dequeue().unsubscribe()

  private def addObserver(observer: Observer[ScriptEvent]): Unit = {
    unsubscribeAll()
    Option(observer).foreach(o => subscriptions.enqueue(eventSubject.subscribe(o)))
  }

  def common: Receive = {
    case Stop =>
      unsubscribeAll()
      Option(logSubscription).foreach(_.unsubscribe())
      Option(vmStdinWriter).foreach(_.close())
      try Option(host).foreach(_.virtualMachine.process().destroy()) catch {
        case NonFatal(t) => log.error("Failed to destroy process", t)
      }
      Option(scriptTimeoutHandler).foreach(_.cancel())
      context.stop(self)

    case GetProgress =>
      sender ! ProgressResponse(summarizeProgress())

    case ReportProgress(p) =>
      reportProgress(p)

    case ObserveScriptEvents(obs) =>
      val sub = eventSubject.subscribe(new Observer[ScriptEvent] {
        override def onNext(item: ScriptEvent): Unit = {
          Option(scriptTimeoutHandler).foreach(_.postpone())
          log.debug("Event observer got item: " + item)
          obs.onNext(item)
        }

        override def onError(error: Throwable): Unit = {
          Option(scriptTimeoutHandler).foreach(_.postpone())
          log.error("Event observer got error", error)
          obs.onError(error)
        }

        override def onComplete(): Unit = obs.onComplete()
      })
      sender ! ObserveScriptEventsResponse(sub)
  }

  private def bumpStartTimeout: Unit = Option(startSender) match {
    case Some(_) =>
      Option(startTimeoutFuture).foreach(_.cancel())
      startTimeoutFuture = DelayedFuture(startTimeout) {
        Option(startSender).foreach { s =>
          s ! StartError(summarizeProgress(), Some(new TimeoutException("Timed out waiting for VM to start")))
          self ! Stop
        }
      }
    case None => // noop
  }

  private def cancelStartTimeout: Unit = {
    Option(startTimeoutFuture).foreach(_.cancel())
    startTimeoutFuture = null
    startTimeout = null
  }

  override def receive: Receive = common orElse {
    case Start(javaHome, extraArgs, timeout) =>
      Try(launchVm(javaHome, extraArgs)) match {
        case Success(vm) =>
          virtualMachine = vm
          // Save for later use
          startSender = sender

          vmStdinWriter = new PrintWriter(new OutputStreamWriter(vm.process().getOutputStream()), true)

          context.become(vmStarted)
          captureLogs()

          startTimeout = timeout
          bumpStartTimeout

        case Failure(NonFatal(t)) =>
          sender ! StartError("", Some(t))
          self ! Stop
        case Failure(t) => throw t // fatal
      }
  }

  def vmStarted: Receive = common orElse {
    case StdOut(output) =>
      bumpStartTimeout // Life sign
      if (output == Signals.ready) {
        // When we receive "ready", the VM is ready to listen for "go".
        reportProgress("Got the ready signal from the VM")
        self ! VmIsReady
      } else {
        reportProgress("VM output: " + output)
      }
    case StdErr(error) =>
      reportProgress("VM error: " + error)
    case HostInitComplete =>
      // Host initialization is complete, so let ScriptExecutor know that it can continue.
      reportProgress("host initialization complete, sending the 'go' signal to the VM")

      context.become(upAndRunning)
      sendToVm(Signals.go)
      startSender ! Started(host)
      startSender = null
      cancelStartTimeout

    case VmIsReady =>
      bumpStartTimeout // Life sign
      reportProgress("VM is ready, connecting the debugger")
      val debugger = new NashornDebugger()
      host = debugger.create(virtualMachine)(context.system)
      setupHost(host)
  }

  def upAndRunning: Receive = common orElse {
    case StdOut(output) =>
      if (output == Signals.scriptDone) {
        scriptSender ! ScriptExecutionDone
        scriptSender = null
        scriptTimeoutHandler.cancel()
        scriptTimeoutHandler = null
      } else {
        reportProgress("VM output: " + output)
        Option(scriptTimeoutHandler).foreach(_.postpone())
      }
    case StdErr(error) =>
      reportProgress("VM error: " + error)
      Option(scriptTimeoutHandler).foreach(_.postpone())

    case ExecuteScript(script, observer, timeout) if scriptSender == null =>
      clearProgress()
      reportProgress("Sending script to VM!")
      scriptSender = sender
      sender ! ScriptWillExecute

      val capturedSelf = self
      scriptTimeoutHandler = new TimeoutHandler(timeout, {
        Option(scriptSender).foreach { s =>
          s ! ScriptFailure(s"Timed out waiting for the script (for ${timeout.toMillis} ms): Progress:\n" + summarizeProgress())
          capturedSelf ! Stop
        }
      })

      addObserver(observer)
      sendToVm(script, encodeBase64 = true)

    case ExecuteScript(_, _, _) =>
      sender ! ScriptFailure("outstanding script not done")
  }

  private def setupHost(host: NashornScriptHost): Unit = {
    reportProgress("VM is running, setting up host")
    hostEventSubscription = host.events.subscribe(new Observer[ScriptEvent] {
      override def onError(error: Throwable): Unit = self ! HostFailure(error)

      override def onComplete(): Unit = self ! HostFailure(new Exception("complete"))

      override def onNext(item: ScriptEvent): Unit = item match {
        case InitialInitializationComplete => self ! HostInitComplete
        case other =>
          log.debug("Dispatching to event observers: " + other)
          eventSubject.onNext(other)
      }
    })
    host.pauseOnBreakpoints()
  }

  private def sendToVm(data: String, encodeBase64: Boolean = false): Unit = {
    val escaped = data.replace("\\", "\\\\")
    val dataToSend = if (encodeBase64) StringUtils.toBase64(escaped) else escaped
    log.info("Sending to VM: " + dataToSend)
    vmStdinWriter.println(dataToSend)
  }

  private def launchVm(javaHome: Option[String], extraNashornArgs: Seq[String]): VirtualMachine = {
    val conn = findLaunchingConnector()
    val args = conn.defaultArguments()
    val homeArg = args.get("home")
    val currentHome = homeArg.value()

    val classPath = System.getProperty("java.class.path")
    val cp = javaHome match {
      case Some(jh) =>
        val pathSeparator = System.getProperty("path.separator")
        classPath.split(pathSeparator).map { part =>
          if (part.startsWith(currentHome)) part.replace(currentHome, jh)
          else part
        }.mkString(pathSeparator)
      case None => classPath
    }
    javaHome.foreach(homeArg.setValue)

    val className = scriptExecutor.getClass.getName.replaceAll("\\$$", "")
    val mainArg = args.get("main")
    val nashornArgs = (Seq("language=es6") ++ extraNashornArgs).map(prefixAsArg).mkString(" ")
    mainArg.setValue(s""""-Dnashorn.args=$nashornArgs" -cp "$cp" $className""")

    def logVirtualMachineOutput(s: String) = self ! StdOut(s)
    def logVirtualMachineError(s: String) = self ! StdErr(s)

    measure(d => log.info(s"Started test VM in ${d.toMillis} ms")) {
      val vm = conn.launch(args)
      new StreamReadingThread(vm.process().getInputStream(), logVirtualMachineOutput).start()
      new StreamReadingThread(vm.process().getErrorStream(), logVirtualMachineError).start()

      waitUntilStarted(vm)
      vm
    }
  }

  private def measure[R](handler: Duration => Unit)(f: => R): R = {
    val before = System.nanoTime()
    try f finally {
      val elapsed = (System.nanoTime() - before).nanos
      handler(elapsed)
    }
  }

  private def prefixAsArg(x: String) = if (x.startsWith("--")) x else "--" + x

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

  private def captureLogs(): Unit = {
    logSubscription = MemoryAppender.logEvents.subscribe(Observer.from {
      case msg if !msg.contains("TRACE") =>
        self ! ReportProgress(msg.trim())
    })
  }

  private def findLaunchingConnector(): LaunchingConnector = Bootstrap.virtualMachineManager().defaultConnector()
}

class TimeoutHandler(timeout: FiniteDuration, timeoutHandler: => Unit)
                    (implicit executionContext: ExecutionContext) {
  private var future: CancellableFuture[_] = _

  def postpone(): Unit = {
    cancel()
    future = DelayedFuture(timeout)(timeoutHandler)
  }

  def cancel(): Unit = {
    Option(future).foreach(_.cancel())
    future = null
  }
}