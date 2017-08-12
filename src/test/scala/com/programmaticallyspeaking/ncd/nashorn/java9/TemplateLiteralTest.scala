package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.host.SimpleValue
import com.programmaticallyspeaking.ncd.nashorn.RealMarshallerTestFixture
import org.scalatest.Inside

class TemplateLiteralTest extends RealMarshallerTestFixture with RunningJava9 with Inside {
  "Marshalling of template literals" - {

    "works for a plain one" in {
      evaluateExpression("`foo`") { (_, actual) =>
        actual should be (SimpleValue("foo"))
      }
    }

    "works for one with an embedded string" in {
      evaluateExpression("(function (x) { return `foo${x}`; })(42)") { (_, actual) =>
        actual should be (SimpleValue("foo42"))
      }
    }

    "works for a multiline lstring" in {
      evaluateExpression("`foo\nbar`") { (_, actual) =>
        actual should be (SimpleValue("foo\nbar"))
      }
    }
  }
}
