package com.programmaticallyspeaking.ncd.infra

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class ScriptURLTest extends UnitTest with TableDrivenPropertyChecks {

  val fromPathCases =
    Table(
      ("desc", "input", "output"),
      ("Windows path", "c:\\temp\\test.txt","file:///c:/temp/test.txt"),
      ("Unix path", "/tmp/test.txt", "file:///tmp/test.txt"),
      ("Windows path on Unix form", "/c:/tmp/test.txt", "file:///c:/tmp/test.txt"),
      ("URL-like non-file path", "eval:/foo/bar", "eval:///foo/bar"),
      ("URL-like file path", "file:/foo/bar", "file:///foo/bar")
    )
  "fromPath" - {
    forAll(fromPathCases) { (desc, input, output) =>
      s"handles $desc" in {
        val sut = ScriptURL.fromPath(input)
        sut.toString should be (output)
      }
    }
  }

  "toFile" - {
    "returns a file with an appropriate path" in {
      val original = "c:\\temp\\test.txt"
      val sut = ScriptURL.fromPath(original)
      val f = sut.toFile

      // Cannot test path accurately on both Windows and Unix, so do a round-trip.
      val sut2 = ScriptURL.fromPath(f.getAbsolutePath)
      sut2.toString should be ("file:///c:/temp/test.txt")
    }
  }

  "isFile" - {
    "returns true for a file URL" in {
      val sut = ScriptURL.fromPath("/tmp/test.txt")
      sut.isFile should be (true)
    }

    "returns false for a non-file URL" in {
      val sut = ScriptURL.fromPath("http://localhost/test.txt")
      sut.isFile should be (false)
    }
  }
}
