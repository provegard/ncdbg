package com.programmaticallyspeaking.ncd.config

import com.programmaticallyspeaking.ncd.testing.UnitTest

class ConfTest extends UnitTest {

  case class Arguments(args: String*) {
    val conf = new Conf(args)
  }

  "NCDbg configuration" - {

    "without arguments" - new Arguments {

      "has a default listen address" in {
        conf.listen() should be (Address("localhost", 7778))
      }

      "has a default debug-connect address" in {
        conf.connect() should be (Address("localhost", 7777))
      }
    }

    "with --listen" - {

      "supports port only" in new Arguments("--listen", "9999") {
        conf.listen() should be (Address("localhost", 9999))
      }

      "supports host:port" in new Arguments("--listen", "somehost:9999") {
        conf.listen() should be (Address("somehost", 9999))
      }
    }

    "with --connect" - {

      "supports port only" in new Arguments("--connect", "9999") {
        conf.connect() should be (Address("localhost", 9999))
      }

      "supports host:port" in new Arguments("--connect", "somehost:9999") {
        conf.connect() should be (Address("somehost", 9999))
      }
    }
  }
}
