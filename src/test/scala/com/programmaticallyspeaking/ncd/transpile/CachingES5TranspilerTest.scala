package com.programmaticallyspeaking.ncd.transpile

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer

import scala.collection.mutable.ListBuffer

class CachingES5TranspilerTest extends UnitTest with MockitoSugar {

  val transpilerCalledWith = ListBuffer[String]()

  def fakeTranspiler: ES5Transpiler = {
    transpilerCalledWith.clear()
    val t = mock[ES5Transpiler]
    when(t.transpile(any[String])).thenAnswer(new Answer[String] {
      override def answer(invocation: InvocationOnMock): String = {
        val input = invocation.getArgument[String](0)
        transpilerCalledWith += input
        "transpiled:" + input
      }
    })
    t
  }

  def createSut = new CachingES5Transpiler(fakeTranspiler)

  "A caching ES5 transpiler" - {
    "delegates to the real one" in {
      val caching = createSut
      caching.transpile("input") should be ("transpiled:input")
    }

    "delegates for each unknown input" in {
      val caching = createSut
      caching.transpile("input1")
      caching.transpile("input2")
      transpilerCalledWith should be (Seq("input1", "input2"))
    }

    "caches and reuses ouput" in {
      val caching = createSut
      caching.transpile("input1")
      caching.transpile("input1")
      transpilerCalledWith should be (Seq("input1"))
    }
  }

}
