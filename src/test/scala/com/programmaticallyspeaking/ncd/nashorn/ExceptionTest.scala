package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import org.scalatest.prop.TableDrivenPropertyChecks

class ExceptionTest extends BreakpointTestFixture with TableDrivenPropertyChecks {

  val exceptionTests = Table(
    ("desc", "thrownValue"),
    ("Error", "new Error('oops')"),
    ("value", "42")
  )

  "when an exception is thrown" - {
    forAll(exceptionTests) { (desc, value) =>
      s"a caught $desc should be detected" in {
        val script =
          s"""try {
             |  throw $value; // row 2
             |} catch (e) {
             |  if (false) debugger; // waitForBreakpoint requires it
             |}
           """.stripMargin

        waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.Caught)) { (_, breakpoint) =>
          lineNumber(breakpoint) should be (2)
        }
      }
    }

    "the emitted breakpoint" - {
      lazy val breakpoint = {
        var theBp: HitBreakpoint = null
        val script =
          """try {
            |  throw "test";
            |} catch (e) {
            |  if (false) debugger; // waitForBreakpoint requires it
            |}
          """.stripMargin
        waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.Caught)) { (_, breakpoint) =>
          theBp = breakpoint
        }
        theBp
      }

      "with Exception reason" in {
        breakpoint.reason.getClass should be (classOf[BreakpointReason.Exception])
      }

      "with no breakpoint ID" in {
        breakpoint.breakpointId should be ('empty)
      }

      "with Exception data" in {
        breakpoint.reason match {
          case BreakpointReason.Exception(data) =>
            data.map(_.getClass) should be (Some(classOf[ErrorValue]))

          case other => fail("Unexpected reason: " + other)
        }
      }
    }

    "an event is emitted for an uncaught error even if pausing is turned off" in {
      val script =
        """var runf = function() {
          |  throw "oops"; // uncaught
          |};
          |var runiface = new java.lang.Runnable({run: runf});
          |var thread = new java.lang.Thread(runiface);
          |thread.start();
          |thread.join();
        """.stripMargin

      waitForEvent(script, _.setSkipAllPauses(true)) {
        case ev: UncaughtError =>
          ev.error.data.message should include ("oops")
      }
    }
  }

  "should stop on an uncaught error (although it's caught and re-thrown in a Java adapter)" in {
    val script =
      """var runf = function() {
        |  throw "oops"; // stop here
        |  debugger; // required
        |};
        |var runiface = new java.lang.Runnable({run: runf});
        |var thread = new java.lang.Thread(runiface);
        |thread.start();
        |thread.join();
      """.stripMargin

    waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.Uncaught)) { (_, breakpoint) =>
      lineNumber(breakpoint) should be (2)
    }
  }

  "should stop on an uncaught reference error" in {
    val script =
      """var f = function() {
        |  return foo + 1; // stop here
        |  debugger; // required
        |};
        |f();
      """.stripMargin

    waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.Uncaught)) { (_, breakpoint) =>
      lineNumber(breakpoint) should be (2)
    }
  }

  "when the error is caught but a Java adapter is between the throw and catch locations, meaning that we don't really know caught-ness" - {
    val script =
      """var runf = function() {
        |  throw "oops"; // stop here
        |  debugger; // required
        |};
        |var runiface = new java.lang.Runnable({run: runf});
        |try {
        |  runiface.run();
        |} catch (e) {}
      """.stripMargin

    "should stop when all exceptions are paused on" in {
      waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.All)) { (_, breakpoint) =>
        lineNumber(breakpoint) should be (2)
      }
    }

    "should stop when uncaught exceptions are paused on, to not miss the exception" in {
      waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.Uncaught)) { (_, breakpoint) =>
        lineNumber(breakpoint) should be (2)
      }
    }
  }

  "should ignore a caught error when pausing on uncaught ones" in {
    val script =
      """try {
        |  throw "oops";
        |} catch (e) {}
        |debugger;
      """.stripMargin

    waitForBreakpoint(script, _.pauseOnExceptions(ExceptionPauseType.Uncaught)) { (_, breakpoint) =>
      lineNumber(breakpoint) should be (4)
    }
  }


  private def lineNumber(bp: HitBreakpoint) = bp.stackFrames.headOption.map(_.location.lineNumber1Based).getOrElse(-1)
}
