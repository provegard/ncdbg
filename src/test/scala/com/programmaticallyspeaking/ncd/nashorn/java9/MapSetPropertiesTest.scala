package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor
import com.programmaticallyspeaking.ncd.nashorn.{NashornDebuggerHost, NashornScriptHost, ObjectPropertyTesting, RealMarshallerTestFixture}

class MapSetPropertiesTest extends RealMarshallerTestFixture with RunningJava9 with ObjectPropertyTesting {

  def ownPropsOf(expr: String): Seq[(String, ObjectPropertyDescriptor)] = {
    var retVal: Seq[(String, ObjectPropertyDescriptor)] = Seq.empty
    testProperties(expr, onlyOwn = true) { props =>
      retVal = props
    }
    retVal
  }

  "The prototype of a Set" - {
    val expr = "(new Set([1,2,3])).__proto__"

    "should have a property descriptor for size" in {
      testProperties(expr, onlyOwn = true) { props =>
        val propNames = props.toMap.keys
        propNames should contain ("size")
      }
    }
  }

  "The prototype of a Map" - {
    val expr = "(new Map([[1,2], [2,3]])).__proto__"
    lazy val propsOfMap = ownPropsOf(expr)

    "should NOT have an internal property for the entries" in {
      val propNames = propsOfMap.toMap.keys
      propNames should not contain ("[[Entries]]")
    }
  }

  "A Map" - {
    val expr = "new Map([[1,2], [2,3]])"
    lazy val propsOfMap = ownPropsOf(expr)

    "should have an internal property for the entries" in {
      val propNames = propsOfMap.toMap.keys
      propNames should contain ("[[Entries]]")
    }

    "should make the [[Entries]] property an array (rather than an iterator)" in {
      val desc = propsOfMap.find(_._1 == "[[Entries]]").head._2
      desc.value.collect { case ArrayNode(size, _, _) => size } should be (Some(2))
    }

    "doesn't expose hidden properties the second time properties are fetched" in {
      evaluateExpression(expr) {
        case (host, c: ComplexNode) =>
          host.asInstanceOf[NashornScriptHost].disableObjectPropertiesCache()

          val props1 = host.getObjectProperties(c.objectId, onlyOwn = true, onlyAccessors = false)
          val props2 = host.getObjectProperties(c.objectId, onlyOwn = true, onlyAccessors = false)

          val names1 = props1.map(_._1)
          val names2 = props2.map(_._1)
          names2 should be (names1)

        case (_, other) => fail("Unknown: " + other)
      }
    }

    "has entries marshalled as a special MapSetEntryNode type" in {
      testEntries(expr) { entriesProps =>
        val item0VN = entriesProps.find(_._1 == "0").flatMap(_._2.value)
        item0VN match {
          case Some(obj: MapSetEntryNode) =>
            obj.key should be (Some(SimpleValue(1)))
            obj.value should be (SimpleValue(2))

          case other => fail("Unexpected [[Entries]] item type: " + other)
        }
      }
    }

    "doesn't include the __proto__ property for a Map/Set entry object" in {
      testEntries(expr) { entriesProps =>
        val item0VN = entriesProps.find(_._1 == "0").flatMap(_._2.value)
        item0VN match {
          case Some(cn: ComplexNode) =>
            val entryProps = getHost.getObjectProperties(cn.objectId, onlyOwn = true, onlyAccessors = false)
            val names = entryProps.map(_._1)
            names should not contain ("__proto__")

          case other => fail("Unexpected [[Entries]] item type: " + other)
        }
      }
    }
  }

  private def testEntries(expr: String)(handler: Seq[(String, ObjectPropertyDescriptor)] => Unit) = {
    testProperties(expr, onlyOwn = true) { props =>
      val desc = props.find(_._1 == "[[Entries]]").head._2
      val entriesObjectId = desc.value.get.asInstanceOf[ComplexNode].objectId

      val entriesProps = getHost.getObjectProperties(entriesObjectId, onlyOwn = true, onlyAccessors = false)
      handler(entriesProps)
    }
  }
}
