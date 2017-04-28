package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ComplexNode
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
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
    ("JSObject array (slot with misbehaving getMember)", s"createInstance('${classOf[SlotBasedArrayJSObjectThatMisbehavesForGetMember].getName}')", Map("0" -> "a", "1" -> "b", "length" -> 2)),
    ("JSObject object", s"createInstance('${classOf[ObjectLikeJSObject].getName}')", Map("a" -> 42, "b" -> 43)),
    ("Scala instance with val", s"createInstance('${classOf[ClassWithVal].getName}')", Map("foo" -> "bar")),
    ("Scala instance with var", s"createInstance('${classOf[ClassWithVar].getName}')", Map("foo" -> "var")),
    ("Scala instance with private val", s"createInstance('${classOf[ClassWithPrivateVal].getName}')", Map("foo" -> "priv-val")),
    ("Scala instance with JavaBeans property", s"createInstance('${classOf[ClassWithJavaBeans].getName}')", Map("fooBar" -> Map("get" -> "<function>", "set" -> "<function>"), "_foo" -> "bar")),
    ("Scala instance with JavaBeans property (no set)", s"createInstance('${classOf[ClassWithJavaBeansOnlyGet].getName}')", Map("foo" -> Map("get" -> "<function>"), "_foo" -> "bar")),
    ("Scala instance with JavaBeans property (no get)", s"createInstance('${classOf[ClassWithJavaBeansOnlySet].getName}')", Map("foo" -> Map("set" -> "<function>"), "_foo" -> "bar")),
    ("Scala instance with inherited JavaBeans property", s"createInstance('${classOf[JavaBeansSubClass].getName}')", Map.empty),
    ("Hashtable-based object", s"createInstance('${classOf[HashtableDerivate].getName}')", Map("foo" -> "bar", "bar" -> "baz")),
    ("Hashtable-based object with complex keys and int values", s"createInstance('${classOf[HashtableComplexKeysIntValues].getName}')", Map("foo" -> 1, "bar" -> 2))
  )

  val complexValuesAlsoInherited = Table(
    ("desc", "expression", "expected"),
    ("Scala instance with inherited field", s"createInstance('${classOf[SubClass].getName}')", Map("foo" -> "priv-val", "sub" -> "qux"))
  )

  def testProperties(clazz: Class[_])(handler: (Map[String, ObjectPropertyDescriptor] => Unit)) = {
    val expr = s"createInstance('${clazz.getName}')"
    evaluateExpression(expr) {
      case (host, c: ComplexNode) =>
        val props = host.getObjectProperties(c.objectId, false, false)

        handler(props - "class")

      case (host, other) => fail("Unknown: " + other)
    }
  }

  "Object property expansion works for" - {
    forAll(complexValues) { (desc, expr, expected) =>
      desc + " (only own)" in {
        evaluateExpression(expr) { (host, actual) =>
          expand(host, actual) should equal (expected)
        }
      }
    }

    forAll(complexValuesAlsoInherited) { (desc, expr, expected) =>
      desc + " (also inherited)" in {
        evaluateExpression(expr) { (host, actual) =>
          expand(host, actual, true) should equal (expected)
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

    "Scala object" - {
      "should mark a val as non-writable" in {
        testProperties(classOf[ClassWithVal]) { props =>
          props.find(_._1 == "foo").map(_._2.isWritable) should be (Some(false))
        }
      }

      "should mark a var as writable" in {
        testProperties(classOf[ClassWithVar]) { props =>
          props.find(_._1 == "foo").map(_._2.isWritable) should be (Some(true))
        }
      }

      "should mark an own property as own" in {
        testProperties(classOf[SubClass]) { props =>
          props.find(_._1 == "sub").map(_._2.isOwn) should be (Some(true))
        }
      }

      "should mark an inherited property as not own" in {
        testProperties(classOf[SubClass]) { props =>
          props.find(_._1 == "foo").map(_._2.isOwn) should be (Some(false))
        }
      }

      "should create an accessor property for a JavaBeans property" in {
        testProperties(classOf[ClassWithJavaBeans]) { props =>
          props.find(_._1 == "fooBar").map(_._2.descriptorType) should be (Some(PropertyDescriptorType.Accessor))
        }
      }
    }

    "Object modification" - {
      "should be visible after first property evaluation (i.e. not hidden by the cache)" in {
        evaluateExpression("{'a':'b'}") {
          case (host, c: ComplexNode) =>
            // Prime the cache, then evaluate
            val ignored = expand(host, c)
            host.evaluateOnStackFrame("$top", "$foo['a']='c'", Map("$foo" -> c.objectId))

            expand(host, c) should be (Map("a" -> "c"))

          case (_, other) => fail("Unknown: " + other)
        }
      }
    }
  }

}

class ClassWithVal {
  val foo = "bar"
}

class ClassWithVar {
  var foo = "var"
}

class ClassWithPrivateVal {
  private val foo = "priv-val"
}

class SubClass extends ClassWithPrivateVal {
  private val sub = "qux"
}

class ClassWithJavaBeans {
  private var _foo = "bar"
  def getFooBar() = _foo
  def setFooBar(s: String): Unit = _foo = s
}

class JavaBeansSubClass extends ClassWithJavaBeans

class ClassWithJavaBeansOnlyGet {
  private val _foo = "bar"
  def getFoo() = _foo
}

class ClassWithJavaBeansOnlySet {
  private var _foo = "bar"
  def setFoo(s: String): Unit = _foo = s
}

class HashtableDerivate extends java.util.Hashtable[Object, Object] {
  put("foo", "bar")
  put("bar", "baz")
}

class HashtableComplexKeysIntValues extends java.util.Hashtable[Key, Int] {
  put(new Key("foo"), 1)
  put(new Key("bar"), 2)
}

class Key(s: String) {
  override def toString: String = s
}