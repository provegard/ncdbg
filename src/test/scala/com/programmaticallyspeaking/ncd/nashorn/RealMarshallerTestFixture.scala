package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{HitBreakpoint, ScriptEvent, ValueNode}
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise}

trait RealMarshallerTestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global
  override val resultTimeout: FiniteDuration = 5.seconds


  protected def evaluateExpression(expr: String)(tester: (ValueNode) => Unit): Unit = {
    val wrapped =
      s"""|(function (result) {
          |debugger;
          |})($expr);
         """.stripMargin
    val resultPromise = Promise[ValueNode]()
    val observer = new Observer[ScriptEvent] {
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

      override def onError(error: Throwable): Unit = resultPromise.tryFailure(error)

      override def onComplete(): Unit = {}
    }
    runScriptWithObserverSync(wrapped, observer) { host =>
      resultPromise.future.map(node => {
        try tester(node) finally {
          host.resume()
        }
      })
    }
  }
}
