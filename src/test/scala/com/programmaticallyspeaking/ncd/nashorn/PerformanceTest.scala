package com.programmaticallyspeaking.ncd.nashorn

import java.util.concurrent.TimeUnit

import com.programmaticallyspeaking.ncd.host._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._
import scala.util.{Failure, Success}

// Manual perf tests
class PerformanceTest extends BreakpointTestFixture with TableDrivenPropertyChecks {

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
      val script =
        """
          |var tab = java.lang.System.properties;
          |debugger;
          |tab.toString();
        """.stripMargin
      waitForBreakpoint(script) { (host, breakpoint) =>

        getHost.disableObjectPropertiesCache()

        host.evaluateOnStackFrame("$top", "tab", Map.empty) match {
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

  }

  private def measure(count: Int)(op: => Unit): Unit = {
    val before = System.nanoTime()
    for (i <- 1 to count) { op }
    val elapsed = (System.nanoTime() - before).nanos
    println("ops per second = " + (count / elapsed.toUnit(TimeUnit.SECONDS)))
  }

  private def objectId(vn: ValueNode): ObjectId = vn match {
    case c: ComplexNode => c.objectId
    case _ => throw new IllegalArgumentException(s"$vn doesn't have an object ID")
  }

}
