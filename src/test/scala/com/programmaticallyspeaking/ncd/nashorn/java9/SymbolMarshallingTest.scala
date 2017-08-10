package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.host.SymbolNode
import com.programmaticallyspeaking.ncd.nashorn.RealMarshallerTestFixture
import org.scalatest.Inside

class SymbolMarshallingTest extends RealMarshallerTestFixture with Inside with RunningJava9 {

  "Marshalling to ValueNode works for" - {
    "local Symbol" in {
      evaluateExpression("Symbol('foo')") { (_, actual) =>
        inside(actual) {
          case SymbolNode(desc, _) => desc should be ("Symbol(foo)")
        }
      }
    }

    "global Symbol" in {
      evaluateExpression("Symbol.for('foo')") { (_, actual) =>
        inside(actual) {
          case SymbolNode(desc, _) => desc should be ("Symbol(foo)")
        }
      }
    }

  }
}
