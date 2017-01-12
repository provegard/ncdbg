package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._

import scala.concurrent.duration._

class RealMarshallerTest extends RealMarshallerTestFixture {
  override val resultTimeout: FiniteDuration = 5.seconds

  "asdf" in {
    runTest("'hello world'") { node =>
      node should be (SimpleValue("hello world"))
    }
  }

  "fffff" in {
    runTest("42") { node =>
      node should be (SimpleValue(42))
    }
  }
}
