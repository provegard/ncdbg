package com.programmaticallyspeaking.ncd.nashorn

import javax.script.Compilable

import com.programmaticallyspeaking.ncd.host.{HitBreakpoint, ScriptEvent, ValueNode}
import com.programmaticallyspeaking.ncd.messaging.Observer
import jdk.nashorn.api.scripting.NashornScriptEngineFactory

import scala.concurrent.{Future, Promise}

trait RealMarshallerTestFixture extends NashornScriptHostTestFixture {

  val targetVmClass: Class[_] = MarshallerTarget.getClass

  def runMarshallerTest(expression: String)(handler: (ValueNode) => Unit): Unit = {
    runTestAgainstHost(Seq(expression))(waitForValueNode(_).map(handler))
  }

  def waitForValueNode(host: NashornScriptHost): Future[ValueNode] = {

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