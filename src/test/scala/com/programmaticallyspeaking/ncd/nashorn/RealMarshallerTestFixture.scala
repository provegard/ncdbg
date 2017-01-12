package com.programmaticallyspeaking.ncd.nashorn

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}
import javax.script.Compilable

import com.programmaticallyspeaking.ncd.host.{HitBreakpoint, ScriptEvent, SimpleValue, ValueNode}
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.{ActorTesting, UnitTest}
import com.sun.jdi.{Bootstrap, VirtualMachine}
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.VMStartEvent
import jdk.nashorn.api.scripting.NashornScriptEngineFactory
import org.slf4s.Logging

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

trait RealMarshallerTestFixture extends UnitTest with Logging with ActorTesting {
  type Appender = (String) => Unit

  import scala.collection.JavaConverters._
  import scala.concurrent.ExecutionContext.Implicits.global

  def defaultAppender(s: String) = log.debug("VM output: " + s)

  val resultTimeout: FiniteDuration

  protected def runTest[R](expression: String, appender: Appender = defaultAppender)(handler: (ValueNode) => R): R = {
    val vm = launchVm(expression, appender)
    val f = waitForValueNode(vm).map(handler)
    try Await.result(f, resultTimeout) finally {
      try vm.process().destroy() catch {
        case e: Exception => // ignore
      }
    }
  }

  private def launchVm(expression: String, appender: Appender): VirtualMachine = {
    val conn = findLaunchingConnector()
    val args = conn.defaultArguments()

    val cp = System.getProperty("java.class.path")
    val className = MarshallerTarget.getClass.getName.replaceAll("\\$$", "")
    val mainArg = args.get("main")
    mainArg.setValue(s"""-cp "$cp" $className "$expression"""")

    val vm = conn.launch(args)

    new StreamReadingThread(vm.process().getInputStream(), appender).start()
    new StreamReadingThread(vm.process().getErrorStream(), appender).start()

    waitUntilStarted(vm)
    vm
  }

  private def waitForValueNode(vm: VirtualMachine): Future[ValueNode] = {

    val debugger = new NashornDebugger()
    val host = debugger.create(vm)

    val resultPromise = Promise[ValueNode]

    host.pauseOnBreakpoints()
    host.events.subscribe(new Observer[ScriptEvent] {
      override def onError(error: Throwable): Unit = resultPromise.tryFailure(error)

      override def onComplete(): Unit = resultPromise.tryFailure(new Exception("complete"))

      override def onNext(item: ScriptEvent): Unit = item match {
        case bp: HitBreakpoint =>
          bp.stackFrames.headOption match {
            case Some(sf) =>
              sf.locals.entries.find(_._1 == "result").map(_._2.resolve()) match {
                case Some(node) => resultPromise.success(node)
                case None => resultPromise.tryFailure(new Exception("No 'result' local"))
              }
            case None => resultPromise.tryFailure(new Exception("No stack frame"))
          }

        case _ => // ignore
      }
    })

    resultPromise.future
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

}

object MarshallerTarget extends App {
  println("Version: " + System.getProperty("java.version"))
  val expression = args.head
  val wrapped =
    s"""
       |(function (result) {
       |debugger;
       |})($expression);
     """.stripMargin
  val scriptEngine = new NashornScriptEngineFactory().getScriptEngine
  val compiledScript = scriptEngine.asInstanceOf[Compilable].compile(wrapped)
  println("Compiled the script, entering loop...")
  while (true) {
    Thread.sleep(20)
    compiledScript.eval()
  }
}