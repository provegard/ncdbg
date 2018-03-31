package com.programmaticallyspeaking.ncd.nashorn

import java.util.concurrent.TimeUnit

import com.programmaticallyspeaking.ncd.host._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

// Manual perf tests
class PerformanceTest extends PerformanceTestFixture {

  "extracting properties from a scope object" - {

    "is fast" ignore {
      val script = "debugger;"
      waitForBreakpoint(script) { (host, breakpoint) =>

        getHost.disableObjectPropertiesCache()

        val globalScope = for {
          st <- breakpoint.stackFrames.headOption
          sc <- st.scopeChain.find(_.scopeType == ScopeType.Global)
        } yield sc

        globalScope match {
          case Some(scope) =>

            val objId = objectId(scope.value)

            measure(50) {
              getHost.getObjectProperties(objId, true, false)
            }

          case None => fail("global scope not found")
        }
      }

    }

  }

  "extracting properties from a hashtable object" - {
    "is fast" ignore {
      measureNamedObject("java.lang.System.properties")
    }
  }

  "extracting properties from a JSObject object" - {
    "is fast" ignore {
      measureNamedObject(s"createInstance('${classOf[BigObjectLikeJSObject].getName}')")
    }
  }

  "evaluating code" - {
    val script = "debugger;"
    val expr = "1+2+3"

    "is fast" ignore {
      waitForBreakpoint(script) { (host, breakpoint) =>

        val stackFrameId = breakpoint.stackFrames.head.id
        // ~68
        measure(50) {
          host.evaluateOnStackFrame(stackFrameId, expr, Map.empty)
        }
      }
    }

    "is fast after compilation" ignore {
      waitForBreakpoint(script) { (host, breakpoint) =>

        val stackFrameId = breakpoint.stackFrames.head.id

        host.compileScript(expr, "", persist = false).andThen {
          case _ =>
            // ~64 -> ~122
            measure(50) {
              host.evaluateOnStackFrame(stackFrameId, expr, Map.empty)
            }
        }

      }
    }
  }
}

trait PerformanceTestFixture extends BreakpointTestFixture {

  def measureNamedObject(expr: String): Unit = {
    val name = "obj"
    val script =
      s"""
         |var $name = $expr;
         |debugger;
         |$name.toString();
       """.stripMargin
    waitForBreakpoint(script) { (host, breakpoint) =>

      getHost.disableObjectPropertiesCache()

      host.evaluateOnStackFrame("$top", name, Map.empty) match {
        case Success(vn: ComplexNode) =>

          val objId = objectId(vn)

          measure(200) {
            getHost.getObjectProperties(objId, true, false)
          }


        case Success(other) => fail("Unexpected: " + other)
        case Failure(t) => fail(t)
      }

    }

  }

  def measure(count: Int)(op: => Unit): Unit = {
    val before = System.nanoTime()
    for (i <- 1 to count) { op }
    val elapsed = (System.nanoTime() - before).nanos
    println("ops per second = " + (count / elapsed.toUnit(TimeUnit.SECONDS)))
  }

  def objectId(vn: ValueNode): ObjectId = vn match {
    case c: ComplexNode => c.objectId
    case _ => throw new IllegalArgumentException(s"$vn doesn't have an object ID")
  }

}

class BigObjectLikeJSObject extends ObjectLikeJSObject {
  override val data: Map[String, AnyRef] = (1 to 40).map { i => s"a$i" -> i.asInstanceOf[AnyRef]}.toMap
}