package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class TranspileSupportTest extends UnitTest with TableDrivenPropertyChecks {

  def createSut = new TranspileSupport {}

  val transpileNeeded =
    Table(
      ("desc", "code", "needed"),
      ("generator function", "function *() {}", true),
      ("named generator function", "function *foo() {}", true),
      ("generator function no ws", "function*foo() {}", true),
      ("no generator", "function foo() {}", false)
    )

  "Transpile support (for Runtime)" - {
    forAll(transpileNeeded) { (desc, code, needed) =>
      s"detects transpilation need when: $desc" in {
        val sut = createSut
        val actual = sut.needsTranspile(code)
        actual should be (needed)
      }
    }
  }
}
