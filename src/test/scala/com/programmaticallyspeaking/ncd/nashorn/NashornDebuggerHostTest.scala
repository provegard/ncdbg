package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.testing.UnitTest
import com.sun.jdi.ThreadReference
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks
import org.mockito.Mockito._

class NashornDebuggerHostTest extends UnitTest with TableDrivenPropertyChecks with MockitoSugar {

  val threadNames = Table(
    ("name", "isInfra"),
    ("Finalizer", true),
    ("Reference Handler", true),
    ("Signal Dispatcher", true),
    ("Attach Listener", true),
    ("attach listener", true),
    ("main", false)
  )

  def threadReferenceWithName(name: String) = {
    val t = mock[ThreadReference]
    when(t.name()).thenReturn(name)
    t
  }

  "isInfrastructureThread" - {
    forAll(threadNames) { (name, isInfra) =>
      val verb = if (isInfra) "be" else "not be"
      s"should consider a thread named '$name' to $verb an infrastructure thread" in {
        NashornDebuggerHost.isInfrastructureThread(threadReferenceWithName(name)) should be(isInfra)
      }
    }
  }
}
