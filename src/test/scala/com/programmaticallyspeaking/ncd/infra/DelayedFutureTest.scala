package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.nashorn.FairAmountOfPatience
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

class DelayedFutureTest extends UnitTest with ScalaFutures with FairAmountOfPatience {
  implicit val ec = ExecutionContext.global

  "DelayedFuture" - {
    "delays a task" in {
      val f = DelayedFuture(150 millis)(42)
      whenReady(f) { result =>
        result should be (42)
      }
    }

    "handles exception" in {
      val f = DelayedFuture(150 millis)(throw new RuntimeException("oops"))
      whenReady(f.failed) { ex =>
        ex.getMessage should be ("oops")
      }
    }

    "can be cancelled" in {
      var mut = ""
      val f = DelayedFuture(400 millis) { mut = "it failed" }
      f.cancel()
      Thread.sleep(500)
      mut should be ("")
    }
  }
}
