package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.testing.UnitTest

class StringAnySeqTest extends UnitTest{
  "StringAnySeq" - {
    "should match on a proper Seq" in {
      val aSeq: Seq[(String, Any)] = Map("String" -> 32).toSeq
      val x: Any = aSeq
      x match {
        case StringAnySeq(_) => // ok
        case _ => fail
      }
    }

    "should match on an empty Seq" in {
      val x: Any = Seq.empty
      x match {
        case StringAnySeq(_) => // ok
        case _ => fail
      }
    }

    "should not match on a Seq with a non-string key" in {
      val aSeq: Seq[(Int, Any)] = Map(42 -> 32).toSeq
      val x: Any = aSeq
      x match {
        case StringAnySeq(_) => fail
        case _ => // ok
      }
    }
  }
}
