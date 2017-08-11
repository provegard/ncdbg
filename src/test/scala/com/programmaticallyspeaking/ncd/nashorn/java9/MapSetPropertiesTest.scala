package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.nashorn.{ObjectPropertyTesting, RealMarshallerTestFixture}

class MapSetPropertiesTest extends RealMarshallerTestFixture with RunningJava9 with ObjectPropertyTesting {

  "The prototype of a Set" - {
    val expr = "(new Set([1,2,3])).__proto__"

    "should have a property descriptor for size" in {
      testProperties(expr, onlyOwn = true) { props =>
        val propNames = props.toMap.keys
        propNames should contain ("size")
      }
    }
  }
}
