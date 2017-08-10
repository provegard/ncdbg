package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.nashorn.{ObjectPropertyTesting, RealMarshallerTestFixture}

class SymbolPropertiesTest extends RealMarshallerTestFixture with RunningJava9 with ObjectPropertyTesting {

  "Object with a symbol property" - {
    val expr = "(function (s) { var o = {}; o[s] = 'test'; return o; })(Symbol('foo'))"

    "should have a property descriptor with the symbol description as name" in {
      testProperties(expr, onlyOwn = true) { props =>
        val propNames = props.keys
        propNames should contain ("Symbol(foo)")
      }
    }

    "should have a property descriptor with a symbol node" in {
      testProperties(expr, onlyOwn = true) { props =>
        val symNode = props.find(_._1 == "Symbol(foo)").flatMap(_._2.symbol)
        symNode.map(_.description) should be (Some("Symbol(foo)"))
      }
    }
  }

  //TODO: Symbol + prop same name
  //TODO: Props of Symbol
  //TODO: Symbol __proto__ ??
}
