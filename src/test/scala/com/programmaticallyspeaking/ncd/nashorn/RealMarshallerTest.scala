package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, PropertyDescriptorType, Undefined}
import com.programmaticallyspeaking.ncd.infra.StringAnyMap
import org.scalactic.Equality
import org.scalatest.Inside
import org.scalatest.prop.TableDrivenPropertyChecks

class RealMarshallerTest extends RealMarshallerTestFixture with Inside with TableDrivenPropertyChecks {
  import RealMarshallerTest._

  val simpleValues = Table(
    ("desc", "expression", "expected"),
    ("string", "'hello world'", SimpleValue("hello world")),
    ("concatenated string", "'hello ' + 'world'", SimpleValue("hello world")),
    ("integer value", "42", SimpleValue(42)),
    ("floating-point value", "42.5", SimpleValue(42.5d)),
    ("boolean value", "true", SimpleValue(true)),
    ("null", "null", EmptyNode),
    ("undefined", "undefined", SimpleValue(Undefined)),
    ("NaN", "NaN", SimpleValue(Double.NaN))
  )

  val complexValues = Table(
    ("desc", "expression", "expected"),
    ("array", "[42]", Map("0" -> 42, "length" -> 1)),
    ("object", "{'a':'b'}", Map("a" -> "b")),
    ("RegExp", "/.*/", Map("multiline" -> false, "source" -> ".*", "global" -> false, "lastIndex" -> 0, "ignoreCase" -> false)),
//    ("Java Array",
//      """(function() {
//        |var StringArray = Java.type("java.lang.String[]");
//        |var arr = new StringArray(1);
//        |arr[0] = "testing";
//        |return arr;
//        |})()
//      """.stripMargin, Map("0" -> "testing")),
//    ("Java Iterator",
//      """(function() {
//        |var ArrayList = Java.type("java.util.ArrayList");
//        |var list = new ArrayList(1);
//        |list.add("testing");
//        |return list.iterator();
//        |})()
//      """.stripMargin, Map("0" -> "testing"))
    ("property with get/set",
      """(function() {
        |var obj = {};
        |var foo = 0;
        |Object.defineProperty(obj, "foo", {
        |  get: function () { return foo; },
        |  set: function (value) { foo = value; }
        |});
        |return obj;
        |})()
      """.stripMargin, Map("foo" -> Map("get" -> "<function>", "set" -> "<function>")))
  )

  "Marshalling of simple values works for" - {
    forAll(simpleValues) { (desc, expr, expected) =>
      desc in {
        evaluateExpression(expr) { (_, actual) =>
          actual should equal (expected)
        }
      }
    }

    "Date" in {
      evaluateExpression("new Date(2017,0,21)") { (_, actual) =>
        inside(actual) {
          case DateNode(str, _) =>
            str should fullyMatch regex "Sat Jan 21 2017 00:00:00 [A-Z]{3}[0-9+]{5} (.*)"
        }
      }
    }

    "Error" in {
      evaluateExpression("new TypeError('oops')") { (_, actual) =>
        inside(actual) {
          case ErrorValue(data, isBasedOnThrowable, _) =>
            val stack = "TypeError: oops\n\tat <program> (<eval>:1)"
            data should be (ExceptionData("TypeError", "oops", 1, -1, "<eval>", Some(stack), None))
            isBasedOnThrowable should be (false)
        }
      }
    }

    "Java Exception" - {
      val expr = "(function(){try{throw new java.lang.IllegalArgumentException('oops');}catch(e){return e;}})()"

      def evalException(handler: (ErrorValue) => Unit): Unit = {
        evaluateExpression(expr) { (_, actual) =>
          inside(actual) {
            case err: ErrorValue => handler(err)
          }
        }
      }

      "with appropriate exception data" in {
        evalException { err =>
          err.data.copy(javaStackIncludingMessage = None) should be (ExceptionData("java.lang.IllegalArgumentException", "oops", 3, -1, "<eval>",
            Some("java.lang.IllegalArgumentException: oops"), None))
        }
      }

      "with Java stack trace data" in {
        evalException { err =>
          err.data.javaStackIncludingMessage match {
            case Some(stack) =>
              stack should fullyMatch regex ("(?s)^java\\.lang\\.IllegalArgumentException: oops\n\tat.*<eval>.*".r)

            case None => fail("No Java stack")
          }
        }
      }

      "with a flag indicating it's Throwable based" in {
        evalException { err =>
          err.isBasedOnThrowable should be (true)
        }
      }

      "with a property 'javaStack' (which cannot be evaluated yet...)" in {
        evaluateExpression(expr) { (host, actual) =>
          expand(host, actual) match {
            case StringAnyMap(aMap) =>
              aMap.get("javaStack") match {
                case Some(st: String) =>
                  st should startWith ("java.lang.IllegalArgumentException: oops")
                case Some(st) => fail("Unexpected javaStack: " + st)
                case None => fail("Missing javaStack")
              }

            case other => fail("Unexpected: " + other)
          }
        }
      }
    }

    "RegExp" - {
      // Note: flags 'u' and 'y' are not supported by Nashorn
      // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp
      val expr = "/.*/gim"

      def evalRegexp(handler: (RegExpNode) => Unit): Unit = {
        evaluateExpression(expr) { (_, actual) =>
          inside(actual) {
            case re: RegExpNode => handler(re)
          }
        }
      }

      "with a string representation" in {
        evalRegexp { re =>
          re.stringRepresentation should be ("/.*/gim")
        }
      }
    }
  }

  "Marshalling of complex values works for" - {
    forAll(complexValues) { (desc, expr, expected) =>
      desc in {
        evaluateExpression(expr) { (host, actual) =>
          expand(host, actual) should equal (expected)
        }
      }
    }
  }

}

object RealMarshallerTest {
  def expand(host: ScriptHost, node: ValueNode): Any = {
    def recurse(node: ValueNode): Any = node match {
      case complex: ComplexNode =>
        host.getObjectProperties(complex.objectId, true, false).map(e => e._2.descriptorType match {
          case PropertyDescriptorType.Generic =>
            e._1 -> "???"
          case PropertyDescriptorType.Data =>
            e._2.value match {
              case Some(value) => e._1 -> recurse(value)
              case None => throw new RuntimeException("Incorrect data descriptor, no value")
            }
          case PropertyDescriptorType.Accessor =>
            val props = e._2.getter.map(_ => "get").toSeq ++ e._2.setter.map(_ => "set")
            e._1 -> props.map(p => p -> "<function>").toMap
        })
      case EmptyNode => null
      case SimpleValue(simple) => simple
      case other => throw new Exception("Unhandled: " + other)
    }
    recurse(node)
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