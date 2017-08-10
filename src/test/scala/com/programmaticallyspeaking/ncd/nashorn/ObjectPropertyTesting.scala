package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ComplexNode
import com.programmaticallyspeaking.ncd.host.types.ObjectPropertyDescriptor

trait ObjectPropertyTesting { self: RealMarshallerTestFixture =>

  def testProperties(expr: String, onlyOwn: Boolean = false, onlyAccessors: Boolean = false)(handler: (Seq[(String, ObjectPropertyDescriptor)] => Unit)): Unit = {
    evaluateExpression(expr) {
      case (host, c: ComplexNode) =>
        val props = host.getObjectProperties(c.objectId, onlyOwn, onlyAccessors)

        handler(props.filterNot(_._1 == "class"))

      case (_, other) => fail("Unknown: " + other)
    }
  }

}
