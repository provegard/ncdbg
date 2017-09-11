package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class PathUtilsTest extends UnitTest with TableDrivenPropertyChecks {

  val nameSuffixCases = Table(
    ("path", "suffix", "result"),
    ("/c/temp/file.js", "_x", "/c/temp/file_x.js"),
    ("/c/temp/file", "_x", "/c/temp/file_x"),
    ("/c/te.mp/file", "_x", "/c/te.mp/file_x"),
    ("c:\\te.mp\\file", "_x", "c:\\te.mp\\file_x")
  )

  "PathUtils.insertNameSuffix" - {

    forAll(nameSuffixCases) { (path, suffix, result) =>
      s"should add a suffix to $path" in {
        PathUtils.insertNameSuffix(path, suffix) should be (result)
      }

    }
  }
}
