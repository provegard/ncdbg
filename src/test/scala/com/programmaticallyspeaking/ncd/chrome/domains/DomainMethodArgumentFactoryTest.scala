package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.net.Protocol
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.TryValues

class DomainMethodArgumentFactoryTest extends UnitTest {

  val factory = DomainMethodArgumentFactory

  "DomainMethodArgumentFactory.create" - {
    "with a message with a malformed method" - {
      "should signal the error" in {
        val msg = Protocol.IncomingMessage(1, "Foobar", null)
        val ex = intercept[IllegalArgumentException](factory.create(msg))
        ex.getMessage should include ("Malformed method")
      }
    }

    "with a message without parameters" - {

      "should create a domain-generic 'enable' case object" in {
        // Access Domain.enable first to force-initialize the instance
        val expected = Domain.enable
        val msg = Protocol.IncomingMessage(1, "FooTestDomain.enable", null)
        val result = factory.create(msg)
        result shouldBe theSameInstanceAs (expected)
      }

      "should create a case object" in {
        val msg = Protocol.IncomingMessage(1, "FooTestDomain.bar", null)
        val result = factory.create(msg)
        result shouldBe theSameInstanceAs (FooTestDomain.bar)
      }

      "should refuse to create a case class" in {
        val msg = Protocol.IncomingMessage(1, "FooTestDomain.baz", null)
        val ex = intercept[IllegalArgumentException](factory.create(msg))
        ex.getMessage should include ("arguments are missing")
      }
    }

    "with a message with empty parameters" - {

      "should create a case object" in {
        val msg = Protocol.IncomingMessage(1, "FooTestDomain.bar", Map.empty)
        val result = factory.create(msg)
        result shouldBe theSameInstanceAs (FooTestDomain.bar)
      }
    }

    "with a message with parameters" - {
      "should create a case class" in {
        val msg = Protocol.IncomingMessage(1, "FooTestDomain.baz", Map("a" -> "data", "b" -> 42))
        val result = factory.create(msg)
        result should be (FooTestDomain.baz("data", 42))
      }

      "should refuse to create a case object" in {
        val msg = Protocol.IncomingMessage(1, "FooTestDomain.bar", Map("a" -> "data", "b" -> 42))
        val ex = intercept[IllegalArgumentException](factory.create(msg))
        ex.getMessage should include ("there are arguments")
      }
    }

    "with an unknown domain and/or method" - {
      "should signal the error" in {
        val msg = Protocol.IncomingMessage(1, "FooTestDomain.xyz", null)
        val ex = intercept[IllegalArgumentException](factory.create(msg))
        ex.getMessage should include ("are unknown")
      }
    }
  }
}
