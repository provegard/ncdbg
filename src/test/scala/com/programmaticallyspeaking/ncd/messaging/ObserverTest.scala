package com.programmaticallyspeaking.ncd.messaging

import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.mutable.ListBuffer

class ObserverTest extends UnitTest {

  "Observer.from" - {
    "ignores an unknown message" in {
      val strings = ListBuffer[String]()
      val obs = Observer.from[AnyRef] {
        case s: String => strings += s
      }
      obs.onNext(1.asInstanceOf[AnyRef])
      obs.onNext("test")
      strings should be (Seq("test"))
    }
  }
}
