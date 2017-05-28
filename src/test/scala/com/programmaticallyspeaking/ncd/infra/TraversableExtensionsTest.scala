package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.testing.UnitTest

class TraversableExtensionsTest extends UnitTest {
  import TraversableExtensions._

  "distinctBy" - {
    "ignores irrelevant properties" in {
      val list = Seq(Subject("a", 42), Subject("a", 43), Subject("c", 43))
      val result = list.distinctBy(_.b)
      result should be (Seq(Subject("a", 42), Subject("a", 43)))
    }
  }

  case class Subject(a: String, b: Int)
}
