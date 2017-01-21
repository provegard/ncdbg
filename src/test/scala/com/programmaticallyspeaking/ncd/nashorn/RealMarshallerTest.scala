package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.host.{EmptyNode, SimpleValue}

class RealMarshallerTest extends RealMarshallerTestFixture {

  val marshalledValues = Table(
    ("desc", "expression", "expected"),
    ("string", "'hello world'", SimpleValue("hello world")),
    ("integer value", "42", SimpleValue(42)),
    ("floating-point value", "42.5", SimpleValue(42.5d)),
    ("boolean value", "true", SimpleValue(true)),
    ("null", "null", EmptyNode),
    ("undefined", "undefined", SimpleValue(Undefined)),
    ("NaN", "NaN", SimpleValue(Double.NaN))
  )

  "Marshalling works for" - {
    forAll(marshalledValues) { (desc, expr, expected) =>
      desc in {
        evaluateExpression(expr) { actual =>
          expected match {
            case SimpleValue(d: Double) =>
              actual match {
                case SimpleValue(ad: Double) =>
                  // Required for NaN comparison
                  java.lang.Double.compare(d, ad) should be (0)
                case other => fail(s"Expected $other to be a SimpleValue(Double)")
              }
            case _ =>
              actual should be(expected)
          }
        }
      }
    }
  }

}
