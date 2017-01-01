package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.testing.UnitTest

class StringAnyMapTest extends UnitTest{
  "StringAnyMap" - {
    "should match on a proper map" in {
      val aMap: Map[String, Any] = Map("String" -> 32)
      val x: Any = aMap
      x match {
        case StringAnyMap(theMap) => // ok
        case _ => fail
      }
    }

    "should match on an empty map" in {
      val x: Any = Map.empty
      x match {
        case StringAnyMap(theMap) => // ok
        case _ => fail
      }
    }

    "should not match on a map with a non-string key" in {
      val aMap: Map[Int, Any] = Map(42 -> 32)
      val x: Any = aMap
      x match {
        case StringAnyMap(theMap) => fail
        case _ => // ok
      }
    }
  }
}
