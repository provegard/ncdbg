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

  def testObjectValue(script: String, objName: String)(f: ObjectId => Unit): Unit = {
    evaluateInScript(script)({ (host, stackframes) =>
      host.evaluateOnStackFrame(stackframes.head.id, objName) match {
        case Success(cn: ComplexNode) => f(cn.objectId)
        case Success(other) => fail("Unexpected evaluate result: " + other)
        case Failure(t) => fail("Error", t)
      }
    })
  }

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
    testObjectValue(script, "obj")(f)
  }

  "callFunctionOn" - {

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

    "can access Object in a strict mode function" in {
      val script =
        """
          |function fun() {
          |  'use strict';
          |  var obj = { value: 99 };
          |  debugger;
          |  obj.toString();
          |}
          |fun();
        """.stripMargin
      testObjectValue(script, "obj") { objId =>
        getHost.callFunctionOn("$top", None, "function (x) { return Object.getOwnPropertyNames(x); }", Seq(objId)) match {
          case Success(an: ArrayNode) =>
            an.size should be (1)
          case Success(other) => fail("Unexpected callFunctionOn result: " + other)
          case Failure(t) => fail("Error", t)
        }
      }
    }
  }
}

