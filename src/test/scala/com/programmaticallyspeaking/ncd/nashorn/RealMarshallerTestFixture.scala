package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Promise}

trait RealMarshallerTestFixture extends UnitTest with NashornScriptHostTestFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.global

  protected def evaluateExpression(expr: String)(tester: (ScriptHost, ValueNode) => Unit): Unit = {
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
              val maybeResultLocal = sf.scopeChain.find(_.scopeType == ScopeType.Local).flatMap(s => {
                s.value match {
                  case obj: ObjectNode =>
                    getHost.getObjectProperties(obj.objectId, true, false).find(_._1 == "result").flatMap(_._2.value)

                  case _ => None
                }
              })
              maybeResultLocal match {
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
    observeAndRunScriptAsync(wrapped, observer) { host =>
      resultPromise.future.map(node => {
        try tester(host, node) finally {
          host.resume()
        }
      })
    }
  }
}
