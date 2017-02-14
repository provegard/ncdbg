package com.programmaticallyspeaking.ncd.messaging

import com.programmaticallyspeaking.ncd.testing.UnitTest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SerializedSubjectTest extends UnitTest {

  def createSut = new SerializedSubject[String]
  def collectingObserver(buf: mutable.Buffer[String]): Observer[String] = Observer.from[String](s => buf += s)

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
  }

}
