package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{ScriptAdded, ScriptEvent}
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.mutable.ListBuffer

class ScriptPublisherTest extends UnitTest {

  def sut(collectTo: ListBuffer[ScriptEvent]) = new ScriptPublisher(new CollectingEmitter(collectTo))

  "ScriptPublisher" - {
    "publishes a script as ScriptAdded to the event emitter" in {
      val target = ListBuffer[ScriptEvent]()
      val script = testScript("a")
      sut(target).publish(script)
      target.headOption should be (Some(ScriptAdded(script)))
    }

    "publishes multiple scripts" in {
      val target = ListBuffer[ScriptEvent]()
      val publisher = sut(target)
      publisher.publish(testScript("a"))
      publisher.publish(testScript("b"))
      target.size should be (2)
    }

    "doesn't republish a script with the same ID" in {
      val target = ListBuffer[ScriptEvent]()
      val publisher = sut(target)
      publisher.publish(testScript("a"))
      publisher.publish(testScript("a"))
      target.size should be (1)
    }
  }

  class CollectingEmitter(collectTo: ListBuffer[ScriptEvent]) extends ScriptEventEmitter {
    override def emit(event: ScriptEvent): Unit = collectTo += event
  }

  def testScript(id: String) = new ScriptImpl(ScriptURL.create(""), Array.empty, id)
}
