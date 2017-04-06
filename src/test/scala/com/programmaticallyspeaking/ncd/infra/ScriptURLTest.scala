package com.programmaticallyspeaking.ncd.infra

import java.net.URI

import com.programmaticallyspeaking.ncd.testing.UnitTest

class ScriptURLTest extends UnitTest {

  "fromPath" - {
    "handles a Windows path" in {
      val sut = ScriptURL.fromPath("c:\\temp\\test.txt")
      sut.toString should be ("file:///c:/temp/test.txt")
    }

    "handles a Unix path" in {
      val sut = ScriptURL.fromPath("/tmp/test.txt")
      sut.toString should be ("file:///tmp/test.txt")
    }

    "handles an URL-like non-file path" in {
      val sut = ScriptURL.fromPath("eval:/foo/bar")
      sut.toString should be ("eval:///foo/bar")
    }

    "handles an URL-like file path" in {
      val sut = ScriptURL.fromPath("file:/foo/bar")
      sut.toString should be ("file:///foo/bar")
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
}
