package com.programmaticallyspeaking.ncd.messaging

import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SerializedSubjectTest extends UnitTest {

  def createSut = new SerializedSubject[String]
  def collectingObserver(buf: mutable.Buffer[String]): Observer[String] = Observer.from[String]{case s => buf += s}

  "SerializedSubject" - {
    "should emit an item" in {
      val items = ListBuffer[String]()
      val sut = createSut
      sut.subscribe(collectingObserver(items))
      sut.onNext("testing")
      items should be (Seq("testing"))
    }

    "should not emit an item after completion" in {
      val items = ListBuffer[String]()
      val sut = createSut
      sut.subscribe(collectingObserver(items))
      sut.onComplete()
      sut.onNext("testing")
      items should be ('empty)
    }

    "should not emit an item after error" in {
      val items = ListBuffer[String]()
      val sut = createSut
      sut.subscribe(collectingObserver(items))
      sut.onError(new Exception("oops"))
      sut.onNext("testing")
      items should be ('empty)
    }

    "handles subscription during dispatching" in {
      val items = ListBuffer[String]()
      val sut = createSut
      sut.subscribe(Observer.from[String] {
        case s if s == "initial" =>
          sut.subscribe(collectingObserver(items))
      })
      sut.onNext("initial")
      sut.onNext("testing")
      items should be (Seq("testing"))
    }

    "re-throws a single observer exception" in {
      val sut = createSut
      sut.subscribe(Observer.from[String] {
        case _ => throw new IllegalArgumentException("oops")
      })
      val ex = intercept[IllegalArgumentException](sut.onNext("testing"))
      ex.getMessage should be ("oops")
    }

    "collects multiple observer exceptions as suppressed" in {
      val sut = createSut
      sut.subscribe(Observer.from[String] {
        case _ => throw new IllegalArgumentException("oops")
      })
      sut.subscribe(Observer.from[String] {
        case _ => throw new IllegalArgumentException("uh-oh")
      })
      val ex = intercept[RuntimeException](sut.onNext("testing"))
      ex.getSuppressed.length should be (2)
    }
  }

}
