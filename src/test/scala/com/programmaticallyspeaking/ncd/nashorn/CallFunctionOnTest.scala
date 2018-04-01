package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, Undefined}
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

class CallFunctionOnTest extends EvaluateTestFixture with TableDrivenPropertyChecks {

  private def testSuccess[A](tr: Try[A])(tester: (A) => Unit): Unit = tr match {
    case Success(value) => tester(value)
    case Failure(t) => fail(t)
  }

  "callFunctionOn" - {

    def testObjectValue(f: ObjectId => Unit): Unit = {
      val script =
        """
          |function fun() {
          |  var obj = { value: 42 };
          |  debugger;
          |  obj.toString();
          |}
          |fun();
        """.stripMargin
      evaluateInScript(script)({ (host, stackframes) =>
        host.evaluateOnStackFrame(stackframes.head.id, "obj") match {
          case Success(cn: ComplexNode) => f(cn.objectId)
          case Failure(t) => fail("Error", t)
        }
      })
    }

    "works for access to 'this'" in {
      val funcDecl = "function () { return this.value; }"
      testObjectValue { objId =>
        val retVal = getHost.callFunctionOn("$top", Some(objId), funcDecl, Seq.empty)

        retVal should be(Success(SimpleValue(42)))
      }
    }

    "works with argument" in {
      val funcDecl = "function (x) { return x.value; }"
      testObjectValue { objId =>
        val retVal = getHost.callFunctionOn("$top", None, funcDecl, Seq(objId))

        retVal should be(Success(SimpleValue(42)))
      }
    }
  }
}

