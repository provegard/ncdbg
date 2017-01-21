package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.SimpleValue

class RealMarshallerTest extends RealMarshallerTestFixture {

  val marshalledValues = Table(
    ("desc", "expression", "expected"),
    ("string", "'hello world'", SimpleValue("hello world")),
    ("integer value", "42", SimpleValue(42))
  )

  "Marshalling works for" - {
    forAll(marshalledValues) { (desc, expr, expected) =>
      desc in {
        evaluateExpression(expr) { actual =>
          actual should be(expected)
        }
      }
    }
  }

}
