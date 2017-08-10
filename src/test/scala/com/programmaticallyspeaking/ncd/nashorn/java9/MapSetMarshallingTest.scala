package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.host.{MapNode, SetNode}
import com.programmaticallyspeaking.ncd.nashorn.RealMarshallerTestFixture
import org.scalatest.Inside

class MapSetMarshallingTest extends RealMarshallerTestFixture with Inside with RunningJava9 {

  "Marshalling to MapNode works for" - {
    "Map" in {
      evaluateExpression("new Map([['a',42]])") { (_, actual) =>
        inside(actual) {
          case MapNode(1, false, _) =>
        }
      }
    }

    "WeakMap" in {
      evaluateExpression("new WeakMap([[{},42]])") { (_, actual) =>
        inside(actual) {
          case MapNode(-1, true, _) =>
        }
      }
    }

    "Set" in {
      evaluateExpression("new Set(['a'])") { (_, actual) =>
        inside(actual) {
          case SetNode(1, false, _) =>
        }
      }
    }

    "WeakSet" in {
      evaluateExpression("new WeakSet([{}])") { (_, actual) =>
        inside(actual) {
          case SetNode(-1, true, _) =>
        }
      }
    }
  }
}
