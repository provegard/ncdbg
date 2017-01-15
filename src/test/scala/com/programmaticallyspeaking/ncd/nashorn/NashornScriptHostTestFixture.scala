package com.programmaticallyspeaking.ncd.nashorn

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}

import com.programmaticallyspeaking.ncd.testing.{ActorTesting, UnitTest}
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.VMStartEvent
import com.sun.jdi.{Bootstrap, VirtualMachine}
import org.slf4s.Logging

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

trait NashornScriptHostTestFixture extends UnitTest with Logging with ActorTesting {
  import scala.collection.JavaConverters._

  def logVirtualMachineOutput(output: String) = log.info("VM output: " + output)
  def logVirtualMachineError(error: String) = log.error("VM error: " + error)

  implicit val executionContext: ExecutionContext

  val resultTimeout: FiniteDuration

  val targetVmClass: Class[_]

  protected def runTestAgainstHost[R](targetVmArguments: Seq[String])(handler: (NashornScriptHost) => Future[R]): R = {
    val vm = launchVm(targetVmArguments)
    val debugger = new NashornDebugger()
    val host = debugger.create(vm)
    val f = handler(host)
    try Await.result(f, resultTimeout) finally {
      try vm.process().destroy() catch {
        case e: Exception => // ignore
      }
    }
  }

  private def launchVm(targetVmArguments: Seq[String]): VirtualMachine = {
    val conn = findLaunchingConnector()
    val args = conn.defaultArguments()

    val cp = System.getProperty("java.class.path")
    val className = targetVmClass.getName.replaceAll("\\$$", "")
    val mainArg = args.get("main")
    val quotedArguments = targetVmArguments.mkString("\"", "\" \"", "\"")
    mainArg.setValue(s"""-cp "$cp" $className $quotedArguments""")

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
