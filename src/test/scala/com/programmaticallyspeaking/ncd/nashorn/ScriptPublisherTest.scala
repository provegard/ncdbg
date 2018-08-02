package com.programmaticallyspeaking.ncd.nashorn

import java.nio.charset.StandardCharsets

import com.programmaticallyspeaking.ncd.host.{ScriptAdded, ScriptEvent}
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.InternalScriptAdded
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

class ScriptPublisherTest extends UnitTest {

  def sut(collectTo: ListBuffer[ScriptEvent]) = new ScriptPublisher(new CollectingEmitter(collectTo))

  def isScriptEvent(pf: PartialFunction[ScriptEvent, Boolean]): ScriptEvent => Boolean =
    e => pf.applyOrElse(e, {
      _: ScriptEvent => false
    })

  "ScriptPublisher" - {
    "with a script" - {
      val script = testScript("a")

      "publishes it as ScriptAdded to the event emitter" in {
        val target = ListBuffer[ScriptEvent]()
        sut(target).publish(script)
        target should contain (ScriptAdded(script))
      }

      "publishes it as InternalScriptAdded to the event emitter" in {
        val target = ListBuffer[ScriptEvent]()
        sut(target).publish(script)
        target should contain (InternalScriptAdded(script))
      }

      "publishes InternalScriptAdded _before_ ScriptAdded to ensure internal functions run before the domain actors" in {
        val target = ListBuffer[ScriptEvent]()
        sut(target).publish(script)
        val idxInternal = target.indexWhere(isScriptEvent { case InternalScriptAdded(s) if s.id == script.id => true })
        val idxExternal = target.indexWhere(isScriptEvent { case ScriptAdded(s) if s.id == script.id => true })
        idxInternal should be < (idxExternal)
      }
    }

    "with a script that contains the marker for suppressed publishing" - {
      val script = testScript("a", s"42/*${ScriptPublisher.PreventPublishingMarker}*/")

      "does NOT publish it as ScriptAdded to the event emitter" in {
        val target = ListBuffer[ScriptEvent]()
        sut(target).publish(script)
        target should not contain (ScriptAdded(script))
      }

      "DOES publish it as InternalScriptAdded to the event emitter" in {
        val target = ListBuffer[ScriptEvent]()
        sut(target).publish(script)
        target should contain (InternalScriptAdded(script))
      }
    }

    "publishes multiple scripts" in {
      val target = ListBuffer[ScriptEvent]()
      val publisher = sut(target)
      publisher.publish(testScript("a"))
      publisher.publish(testScript("b"))
      countOfType[ScriptAdded](target) should be (2)
    }

    "doesn't republish a script with the same ID" in {
      val target = ListBuffer[ScriptEvent]()
      val publisher = sut(target)
      publisher.publish(testScript("a"))
      publisher.publish(testScript("a"))
      countOfType[ScriptAdded](target) should be (1)
    }
  }

  private def countOfType[A <: ScriptEvent : ClassTag](list: Seq[ScriptEvent]): Int = {
    val clazz = implicitly[ClassTag[A]].runtimeClass
    list.count(clazz.isInstance)
  }

  class CollectingEmitter(collectTo: ListBuffer[ScriptEvent]) extends ScriptEventEmitter {
    override def emit(event: ScriptEvent): Unit = collectTo += event
  }

  def testScript(id: String, contents: String = "") = {
    val data = contents.getBytes(StandardCharsets.UTF_8)
    new ScriptImpl(ScriptURL.create(""), data, id, 1)
  }
}
