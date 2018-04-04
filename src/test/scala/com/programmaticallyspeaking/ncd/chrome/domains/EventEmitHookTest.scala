package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.concurrent.ScalaFutures

case class EETestParams(value: Int)

class EventEmitHookTest extends TestKit(ActorSystem("eeht")) with UnitTest with ScalaFutures {

  def sut() = new EventEmitHook()
  def createEvent(params: AnyRef, method: String = "foobar") = Messages.Event(method, params)
  def emitEvent(e: Messages.Event, ee: EventEmitHook) = ee.emitEvent(e, testActor)

  "EventEmitHook" - {

    lazy val matchExisting = {
      val ee = sut()
      emitEvent(createEvent(EETestParams(1)), ee)
      emitEvent(createEvent(EETestParams(2)), ee)
      ee.awaitEvent({ case Messages.Event(_, x: EETestParams) => x.value == 1 })
    }

    lazy val matchLater = {
      val ee = sut()
      val f = ee.awaitEvent({ case Messages.Event(_, x: EETestParams) => x.value == 1 })
      emitEvent(createEvent(EETestParams(2)), ee)
      emitEvent(createEvent(EETestParams(1)), ee)
      f
    }

    "matches an existing event right away" in {
      matchExisting.isCompleted should be (true)
    }

    "returns existing matching event(s) in a Future" in {
      whenReady(matchExisting) { events =>
        events should be (List(createEvent(EETestParams(1))))
      }
    }

    "returns a matched event later" in {
      whenReady(matchLater) { events =>
        events should be (List(createEvent(EETestParams(1))))
      }
    }

    "has limited recollection of emitted events (for matching existing)" in {
      val ee = new EventEmitHook(2)
      emitEvent(createEvent(EETestParams(1)), ee)
      emitEvent(createEvent(EETestParams(2)), ee)
      emitEvent(createEvent(EETestParams(3)), ee)
      val f = ee.awaitEvent({ case Messages.Event(_, _: EETestParams) => true })
      whenReady(f) { events =>
        events.map(_.params) should be (Seq(EETestParams(2), EETestParams(3)))
      }
    }
  }
}
