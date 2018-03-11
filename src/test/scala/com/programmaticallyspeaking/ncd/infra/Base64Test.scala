package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.testing.UnitTest

class Base64Test extends UnitTest {

  "Base64" - {
    "encodes a string" in {
      Base64.encodeString("testing") should be ("dGVzdGluZw==")
    }
    "decodes a string" in {
      Base64.decodeString("dGVzdGluZw==") should be ("testing")
    }
  }
}
