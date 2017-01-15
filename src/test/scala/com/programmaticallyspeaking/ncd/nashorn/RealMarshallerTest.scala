package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class RealMarshallerTest extends RealMarshallerTestFixture {
  override val resultTimeout: FiniteDuration = 5.seconds
  implicit val executionContext = ExecutionContext.global

  "Marshalling from an actual VM" - {

    "should handle a string" in {
      runMarshallerTest("'hello world'") { node =>
        node should be(SimpleValue("hello world"))
      }
    }

    "should handle a number" in {
      runMarshallerTest("42") { node =>
        node should be(SimpleValue(42))
      }
    }
  }
}
