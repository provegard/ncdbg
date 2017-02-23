package com.programmaticallyspeaking.ncd.boot

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.rogach.scallop.exceptions.WrongOptionFormat
import org.rogach.scallop.{ScallopConf, throwError}

class AddressConverterTest extends UnitTest {
  // Throw error instead of exiting on option error.
  throwError.value = true

  def conf(args: String*) = new ScallopConf(args.toSeq) {
    val address = opt[Address]()(new AddressConverter)
    verify()
  }

  "should parse host:port" in {
    val c = conf("--address", "foo:1234")
    c.address.toOption should be (Some(Address("foo", 1234)))
  }

  "should parse only port" in {
    val c = conf("--address", "1234")
    c.address.toOption should be (Some(Address("localhost", 1234)))
  }

  "should handle no address" in {
    val c = conf()
    c.address.toOption should be (None)
  }

  "should reject non-integer port" in {
    intercept[WrongOptionFormat](conf("--address", "foo"))
  }
}
