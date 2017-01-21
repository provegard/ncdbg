package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.SimpleValue

class RealMarshallerTest extends RealMarshallerTestFixture {

  val marshalledValues = Table(
    ("expression", "expected"),
    ("'hello world'", SimpleValue("hello world")),
    ("42", SimpleValue(42))
  )

  "Nashorn values can be marshalled" in {
    forAll(marshalledValues) { (expr, expected) =>
      evaluateExpression(expr) { actual =>
        actual should be(expected)
      }
    }
  }

}
