package com.programmaticallyspeaking.ncd.chrome.net

import com.programmaticallyspeaking.ncd.chrome.net.Protocol.EmptyResponse
import com.programmaticallyspeaking.ncd.infra.ObjectMapping
import com.programmaticallyspeaking.ncd.testing.UnitTest

class ProtocolTest extends UnitTest {

  "EmptyResponse" - {
    "should have an empty 'result' object in JSON since VSCode requires it" in {
      val response = EmptyResponse(1)
      val json = ObjectMapping.toJson(response)
      json should include (""""result":{}""")
    }
  }
}
