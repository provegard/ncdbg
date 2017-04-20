package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class ArbitraryObjectPropertyHolderTest extends UnitTest with TableDrivenPropertyChecks {

  val beanNameExtractionCases = Table(
    ("methodName", "propertyName"),
    ("getFoo", Some("foo")),
    ("getFooBar", Some("fooBar")),
    ("setFoo", Some("foo")),
    ("getfoo", None),
    ("random", None)
  )

  "extractJavaBeansPropertyName" - {

    forAll(beanNameExtractionCases) { (methodName, propertyName) =>
      s"extracts $propertyName from $methodName" in {
        val actual = ArbitraryObjectPropertyHolder.extractJavaBeansPropertyName(methodName)
        actual should be (propertyName)
      }

    }
  }
}
