package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.infra.StringAnyMap
import org.scalatest.Inside
import org.scalatest.prop.TableDrivenPropertyChecks

class ObjectPropertiesTest extends RealMarshallerTestFixture with Inside with TableDrivenPropertyChecks {
  import RealMarshallerTest._

  val complexValues = Table(
    ("desc", "expression", "expected"),
    ("array", "[42]", Map("0" -> 42, "length" -> 1)),
    ("object", "{'a':'b'}", Map("a" -> "b")),
    ("RegExp", "/.*/", Map("multiline" -> false, "source" -> ".*", "global" -> false, "lastIndex" -> 0, "ignoreCase" -> false)),
    ("Java Array",
      """(function() {
        |var StringArray = Java.type("java.lang.String[]");
        |var arr = new StringArray(2);
        |arr[0] = "testing";
        |arr[1] = "foobar";
        |return arr;
        |})()
      """.stripMargin, Map("0" -> "testing", "1" -> "foobar", "length" -> 2)),
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
      """.stripMargin, Map("foo" -> Map("get" -> "<function>", "set" -> "<function>"))),
    ("JSObject array (classname)", s"createInstance('${classOf[ClassNameBasedArrayJSObject].getName}')", Map("0" -> "a", "1" -> "b", "length" -> 2)),
    ("JSObject array (isArray)", s"createInstance('${classOf[IsArrayBasedArrayJSObject].getName}')", Map("0" -> "a", "1" -> "b", "length" -> 2)),
    ("JSObject array (slot only)", s"createInstance('${classOf[OnlySlotBasedArrayJSObject].getName}')", Map("0" -> "a", "1" -> "b", "length" -> 2)),
    ("JSObject object", s"createInstance('${classOf[ObjectLikeJSObject].getName}')", Map("a" -> 42, "b" -> 43))
  )

  "Object property expansion works for" - {
    forAll(complexValues) { (desc, expr, expected) =>
      desc in {
        evaluateExpression(expr) { (host, actual) =>
          expand(host, actual) should equal (expected)
        }
      }
    }

    "Java Exception" - {
      val expr = "(function(){try{throw new java.lang.IllegalArgumentException('oops');}catch(e){return e;}})()"

      def getStringProperty(from: Map[String, Any], prop: String): String = from.get(prop) match {
        case Some(st: String) => st
        case Some(st) => fail(s"Unexpected $prop: " + st)
        case None => fail(s"Missing $prop")
      }

      def evaluateException(handler: (Map[String, Any] => Unit)) = {
        evaluateExpression(expr) { (host, actual) =>
          expand(host, actual) match {
            case StringAnyMap(aMap) => handler(aMap)
            case other => fail("Unexpected: " + other)
          }
        }
      }

      "with a property 'javaStack' (which cannot be evaluated yet...)" in {
        evaluateException { aMap =>
          val st = getStringProperty(aMap, "javaStack")
          st should startWith ("java.lang.IllegalArgumentException: oops")
        }
      }

      "with a property 'message' (which cannot be evaluated yet...)" in {
        evaluateException { aMap =>
          val st = getStringProperty(aMap, "message")
          st should startWith ("oops")
        }
      }
    }
  }

}
