package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}
import com.programmaticallyspeaking.ncd.infra.StringAnyMap
import org.scalatest.Inside
import org.scalatest.prop.TableDrivenPropertyChecks

class ObjectPropertiesNoJavaTest extends RealMarshallerTestFixture with Inside with TableDrivenPropertyChecks with ObjectPropertyTesting {

  import RealMarshallerTest._
  override val scriptExecutor: ScriptExecutorBase = ScriptExecutorNoJava

  "Object property expansion when Java isn't around" - {
    "works for a script object" in {
      val expr = "{ foo: 42 }"
      val expected = Map("foo" -> 42)
      evaluateExpression(expr) { (host, actual) =>
        expand(host, actual) should equal (expected)
      }
    }
  }
}

class ObjectPropertiesTest extends RealMarshallerTestFixture with Inside with TableDrivenPropertyChecks with ObjectPropertyTesting {
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

  val complexValuesOnlyAccessors = Table(
    ("desc", "expression", "expected"),
    ("JSObject object", s"createInstance('${classOf[ObjectLikeJSObject].getName}')", Map.empty[String, Any]),
    ("Hashtable-based object", s"createInstance('${classOf[HashtableDerivate].getName}')", Map.empty),
    ("Scala instance with JavaBeans property", s"createInstance('${classOf[ClassWithJavaBeans].getName}')", Map("fooBar" -> Map("get" -> "<function>", "set" -> "<function>"))),
    ("Java Array",
      """(function() {
        |var arr = new (Java.type("java.lang.String[]"))(1);
        |arr[0] = "testing";
        |return arr;
        |})()
      """.stripMargin, Map.empty)
  )

  val complexValuesIncludingProto = Table(
    ("desc", "expression", "expected"),
    ("Script object with prototype",
      """(function() {
        |  var base = { foo: 41 };
        |  var obj = Object.create(base);
        |  obj.bar = 42;
        |  return obj;
        |})()
      """.stripMargin, Map("bar" -> 42, "__proto__" -> Map("foo" -> 41, "__proto__" -> AnyObject)))
  )

  def testProperties(clazz: Class[_])(handler: (Seq[(String, ObjectPropertyDescriptor)] => Unit)): Unit = {
    val expr = s"createInstance('${clazz.getName}')"
    testProperties(expr)(handler)
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
          expand(host, actual, includeInherited = true) should equal (expected)
        }
      }
    }

    forAll(complexValuesOnlyAccessors) { (desc, expr, expected) =>
      desc + " (own, only accessors)" in {
        evaluateExpression(expr) { (host, actual) =>
          expand(host, actual, includeInherited = false, onlyAccessors = true) should equal (expected)
        }
      }
    }

    forAll(complexValuesIncludingProto) { (desc, expr, expected) =>
      desc + " (own, including __proto__)" in {
        evaluateExpression(expr) { (host, actual) =>
          implicit val eq = anyEqWithMapSupport
          expand(host, actual, expandProto = true) should equal (expected)
        }
      }
    }

    "Java NPE" - {
      val expr = "(function(){try{throw new java.lang.NullPointerException();}catch(e){return e;}})()"

      "with an extra/internal property 'Message' which is EmptyNode" in {
        evaluateExpression(expr) { (host, actual) =>
          actual match {
            case cn: ComplexNode =>

              val props = host.getObjectProperties(cn.objectId, true, false)
              val messageProp = props.find(_._1 == "[[Message]]")
              val messageValue = messageProp.flatMap(_._2.value)

              messageValue should be (Some(EmptyNode))

            case other => fail("Unexpected: " + other)
          }
        }
      }
    }

    "Java Exception" - {
      val expr = "(function(){try{throw new java.lang.IllegalArgumentException('oops');}catch(e){return e;}})()"

      def evaluateException(handler: (Map[String, Any] => Unit)) = {
        evaluateExpression(expr) { (host, actual) =>
          expand(host, actual) match {
            case StringAnyMap(aMap) => handler(aMap)
            case other => fail("Unexpected: " + other)
          }
        }
      }

      lazy val expanded = {
        var aMap: Map[String, Any] = null
        evaluateException { m => aMap = m }
        aMap
      }

      "with an extra/internal property 'JavaStack' (which cannot be evaluated yet...)" in {
        val st = getStringProperty(expanded, "[[JavaStack]]")
        st should startWith ("java.lang.IllegalArgumentException: oops")
      }

      "with an extra/internal property 'Message' (which cannot be evaluated yet...)" in {
        val st = getStringProperty(expanded, "[[Message]]")
        st should startWith ("oops")
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
            host.callFunctionOn(StackFrame.TopId, None, "function (x) { x['a'] = 'c'; }", Seq(c.objectId)).get

            expand(host, c) should be (Map("a" -> "c"))

          case (_, other) => fail("Unknown: " + other)
        }
      }
    }

    "Bound function" - {
      val expr = "(function () { function Add(a,b) { return a + b; }; return Add.bind(null, 1); })()"

      "with an internal property 'TargetFunction'" in {
        testProperties(expr) { props =>
          getDescriptorFor(props, "[[TargetFunction]]").value match {
            case Some(FunctionNode(name, _, _)) => name should be ("Add")
            case other => fail("Unexpected: " + other)
          }
        }
      }
    }

    "function with multiple scopes" - {
      val expr =
        """(function f() {
          |  var x = 1;
          |  return x1();
          |
          |  function x1() {
          |    var y = 2;
          |    return y1;
          |
          |    function y1() {
          |      return x + y;
          |    }
          |  }
          |})()
        """.stripMargin

      "and gives multiple internal 'Scopes'" in {
        testProperties(expr) { props =>
          getDescriptorFor(props, "[[Scopes]]").value match {
            case Some(ScopeList(size, _)) => size should be (2)
            case other => fail("Unexpected: " + other)
          }
        }
      }
    }

    "Regular function that captures something" - {
      val expr = "(function (global) { function Add(a) { return a + global.x?0:1; }; return Add; })(this)"

      "with an internal property 'Scopes' of size 1" in {
        testProperties(expr) { props =>
          getDescriptorFor(props, "[[Scopes]]").value match {
            case Some(ScopeList(size, _)) => size should be (1)
            case other => fail("Unexpected: " + other)
          }
        }
      }

      def getScopesProps(host: ScriptHost, props: Seq[(String, ObjectPropertyDescriptor)], onlyAccessors: Boolean) =
        getDescriptorFor(props, "[[Scopes]]").value.map(expand(host, _, onlyAccessors = onlyAccessors)) match {
          case Some(StringAnyMap(aMap)) => aMap
          case other => fail("Unexpected [[Scopes]] expansion: " + other)
        }

      "with an actual scope in 'Scopes'" in {
        evaluateExpression(expr) {
          case (host, c: ComplexNode) =>
            val props = host.getObjectProperties(c.objectId, true, false)
            getScopesProps(host, props, false).get("0") match {
              case Some(head) =>
                head should be (Map("scope" -> true, "name" -> "", "type" -> "closure"))

              case None => fail("no scopes")
            }

          case (_, other) => fail("Unknown: " + other)
        }
      }

      "without Scopes properties if only accessor properties are requested" in {
        evaluateExpression(expr) {
          case (host, c: ComplexNode) =>
            val props = host.getObjectProperties(c.objectId, true, false)
            getScopesProps(host, props, true).size should be (0)

          case (_, other) => fail("Unknown: " + other)
        }

      }
    }

    "Regular function that captures Nothing" - {
      val expr = "(function Add(a,b) { return a + b; })"

      "with an internal property 'Scopes' that is empty" in {
        testProperties(expr) { props =>
          getDescriptorFor(props, "[[Scopes]]").value match {
            case Some(ScopeList(size, _)) => size should be (0)
            case other => fail("Unexpected: " + other)
          }
        }
      }
    }
  }

  def getDescriptorFor(props: Seq[(String, ObjectPropertyDescriptor)], name: String) = props.find(_._1 == name) match {
    case Some((_, desc)) => desc
    case None => fail(s"Missing $name")
  }

  def getStringProperty(from: Map[String, Any], prop: String): String = from.get(prop) match {
    case Some(st: String) => st
    case Some(st) => fail(s"Unexpected $prop: " + st)
    case None => fail(s"Missing $prop (available: ${from.keys.mkString(", ")})")
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