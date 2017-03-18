package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class NashornDebuggerHostTest extends UnitTest with TableDrivenPropertyChecks {

  val cases = Table(
    ("desc", "sourceName", "typeName", "expectedUri"),
    ("an eval script", "<eval>", "jdk.nashorn.internal.scripts.Script$3$\\^eval\\_", "file://jdk/nashorn/internal/scripts/Script_3_/_eval/_"),
    ("a file:// script", "file://some/script.js", "irrelevant.Type", "file://some/script.js"),
    ("a file:/ script", "file:/some/script.js", "irrelevant.Type", "file://some/script.js"),
    ("a script with Windows drive letter", "file://c:/path/script.js", "irrelevant.Type", "file://c/path/script.js"),
    ("a relative path", "some/script.js", "irrelevant.Type", "file://some/script.js")
  )

  "NashornDebuggerHost.scriptPathFromLocation" - {
    forAll(cases) { (desc, sourceName, typeName, expectedUri) =>

      s"should return an URI for $desc" in {
        val actual = NashornDebuggerHost.scriptPathFromLocation(sourceName, typeName)
        actual should be (expectedUri)
      }
    }
  }
}
