package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.testing.UnitTest

class ScriptEventTest extends UnitTest {
  class MyEvent extends ScriptEvent {
    override protected def toStringParams(): Map[String, Any] = Map("foo" -> 42, "bar" -> "test")
  }
  object ObjEvent extends ScriptEvent

  "ScriptEvent.toString" - {
    "includes name + parameters" in {
      val ev = new MyEvent()
      ev.toString should be ("MyEvent(foo=42, bar=test)")
    }

    "excludes $ for object" in {
      val ev = ObjEvent
      ev.toString should be ("ObjEvent()")
    }

  }
}
