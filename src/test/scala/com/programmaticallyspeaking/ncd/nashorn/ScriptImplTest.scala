package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class ScriptImplTest extends UnitTest with TableDrivenPropertyChecks {

  val cases = Table(
    ("desc", "path", "expectedUrl"),
    ("a file:// path", "file://some/script.js", "file://some/script.js"),
    ("a file:/ path", "file:/some/script.js", "file://some/script.js"),
    ("a file:/ path with a Windows drive letter", "file://c:/path/script.js", "file://c/path/script.js"),
    ("a Windows path", "c:\\path\\script.js", "file://c/path/script.js")
  )

  "ScriptImpl.filePathToUrl" - {
    forAll(cases) { (desc, path, expectedUri) =>

      s"should return an URL for $desc" in {
        val actual = ScriptImpl.filePathToUrl(path)
        actual should be (expectedUri)
      }
    }
  }

}
