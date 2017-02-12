package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
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
  }

}
