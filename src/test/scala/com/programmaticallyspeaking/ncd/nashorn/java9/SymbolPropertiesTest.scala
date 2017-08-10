package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.host.SimpleValue
import com.programmaticallyspeaking.ncd.nashorn.{ObjectPropertyTesting, RealMarshallerTestFixture}

class SymbolPropertiesTest extends RealMarshallerTestFixture with RunningJava9 with ObjectPropertyTesting {

  "Object with a symbol property" - {
    val expr = "(function (s) { var o = {}; o[s] = 'test'; return o; })(Symbol('foo'))"

    "should have a property descriptor with the symbol description as name" in {
      testProperties(expr, onlyOwn = true) { props =>
        val propNames = props.toMap.keys
        propNames should contain ("Symbol(foo)")
      }
    }

    "should have a property descriptor with a symbol node" in {
      testProperties(expr, onlyOwn = true) { props =>
        val symNode = props.find(_._1 == "Symbol(foo)").flatMap(_._2.symbol)
        symNode.map(_.description) should be (Some("Symbol(foo)"))
      }
    }

    "should have a symbol property descriptor with a value" in {
      testProperties(expr, onlyOwn = true) { props =>
        val valueNode = props.find(_._1 == "Symbol(foo)").flatMap(_._2.value)
        valueNode should be (Some(SimpleValue("test")))
      }
    }
  }

  "Object with a symbol property and a regular property with the same name" - {
    val expr = "(function (s) { var o = {}; o[s] = 'test'; o[s.toString()] = 'test again'; return o; })(Symbol('foo'))"

    "should return two property descriptors with the same name" in {
      testProperties(expr, onlyOwn = true) { props =>
        val count = props.map(_._1).count(_ == "Symbol(foo)")
        count should be (2)
      }
    }

    "should only have one property descriptor with a symbol" in {
      testProperties(expr, onlyOwn = true) { props =>
        val count = props.map(_._2).count(_.symbol.isDefined)
        count should be (1)
      }
    }
  }

  //TODO: Props of Symbol
  //TODO: Symbol __proto__ ??
}
