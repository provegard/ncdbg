package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.infra.ErrorUtils.TypeAndMessage
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ErrorUtilsTest extends UnitTest {
  def parse(msg: String) = ErrorUtils.parseMessage(msg)

  "ErrorUtils.parseMessage" - {
    "parses type with message" in {
      parse("TypeError: Message") should be (TypeAndMessage("TypeError", "Message"))
    }

    "parses only message" in {
      parse("Message") should be (TypeAndMessage("Error", "Message"))
    }
  }

}
