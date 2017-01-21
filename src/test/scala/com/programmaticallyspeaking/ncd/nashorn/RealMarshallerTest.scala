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
    ("array", "[1]", Internal(Seq("0" -> Leaf(SimpleValue(1)))))
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
  sealed trait ExpandedNode
  case class Leaf(node: ValueNode) extends ExpandedNode
  case class Internal(children: Seq[(String, ExpandedNode)]) extends ExpandedNode

  def expand(node: ValueNode): ExpandedNode = node match {
    case complex: ComplexNode =>
      Internal(complex.entries.map(e => e._1 -> expand(e._2.resolve())))
    case other =>
      Leaf(other)
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