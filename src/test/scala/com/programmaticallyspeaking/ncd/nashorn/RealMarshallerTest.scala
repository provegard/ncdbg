package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.host.{ComplexNode, EmptyNode, SimpleValue, ValueNode}
import org.scalactic.Equality

class RealMarshallerTest extends RealMarshallerTestFixture {
  import RealMarshallerTest._

  val simpleValues = Table(
    ("desc", "expression", "expected"),
    ("string", "'hello world'", SimpleValue("hello world")),
    ("integer value", "42", SimpleValue(42)),
    ("floating-point value", "42.5", SimpleValue(42.5d)),
    ("boolean value", "true", SimpleValue(true)),
    ("null", "null", EmptyNode),
    ("undefined", "undefined", SimpleValue(Undefined)),
    ("NaN", "NaN", SimpleValue(Double.NaN))
  )

  val complexValues = Table(
    ("desc", "expression", "expected"),
    ("array", "[1]", Map("0" -> 1)),
    ("object", "{'a':'b'}", Map("a" -> "b"))
  )

  "Marshalling of simple values works for" - {
    forAll(simpleValues) { (desc, expr, expected) =>
      desc in {
        evaluateExpression(expr) { actual =>
          actual should equal (expected)
        }
      }
    }
  }

  "Marshalling of complex values works for" - {
    forAll(complexValues) { (desc, expr, expected) =>
      desc in {
        evaluateExpression(expr) { actual =>
          expand(actual) should equal (expected)
        }
      }
    }
  }

}

object RealMarshallerTest {
  def expand(node: ValueNode): Any = node match {
    case complex: ComplexNode =>
      complex.entries.map(e => e._1 -> expand(e._2.resolve())).toMap
    case EmptyNode => null
    case SimpleValue(simple) => simple
    case other => throw new Exception("Unhandled: " + other)
  }

  implicit val valueNodeEq: Equality[ValueNode] =
    (a: ValueNode, b: Any) => b match {
      case vn: ValueNode =>
        a match {
          case SimpleValue(d: Double) =>
            vn match {
              case SimpleValue(ad: Double) =>
                // Required for NaN comparison
                java.lang.Double.compare(d, ad) == 0
              case other => false
            }
          case _ =>
            a == vn
        }

      case other => false
    }
}