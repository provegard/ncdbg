package com.programmaticallyspeaking.ncd.infra

import java.io.File
import java.net.URL

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.prop.TableDrivenPropertyChecks

class ScriptURLTest extends UnitTest with TableDrivenPropertyChecks {
  val isCaseInsensitiveFileSystem = new File("A") == new File("a")
  val fromPathCases =
    Table(
      ("desc", "input", "output"),
      ("Windows path", "c:\\temp\\test.txt","file:///c:/temp/test.txt"),
      ("Windows path with forward slashes", "c:/temp/test.txt","file:///c:/temp/test.txt"),
      ("relative Windows path", "temp\\test.txt","temp/test.txt"),
      ("Path with ..", "c:\\temp\\subdir\\..\\test.txt","file:///c:/temp/test.txt"),
      ("Unix path", "/tmp/test.txt", "file:///tmp/test.txt"),
      ("relative Unix path", "tmp/test.txt", "tmp/test.txt"),
      ("Windows path on Unix form", "/c:/tmp/test.txt", "file:///c:/tmp/test.txt"),
      ("URL-like non-file path", "eval:/foo/bar", "eval:///foo/bar"),
      ("file URL without authority", "file:/foo/bar", "file:///foo/bar"),
      ("file URL without authority and ..", "file:/foo/subdir/../bar", "file:///foo/bar"),
      ("file URL with authority", "file:///foo/bar", "file:///foo/bar"),
      ("data URL", "data:application/json;base64,e30=", "data:application/json;base64,e30="),
      ("HTTP URL", "http://localhost/test", "http://localhost/test"),
      ("JAR file", "jar:file:/foo/bar.jar!/dir/file.js", "file:///foo/bar.jar!/dir/file.js")
    )

  val resolveCases =
    Table(
      ("desc", "original", "input", "output"),
      ("relative Windows path", "c:\\temp\\test.txt", "bar.txt", "c:\\temp\\bar.txt"),
      ("absolute Windows path", "c:\\temp\\test.txt", "c:\\files\\data.txt", "c:\\files\\data.txt"),
      ("absolute Unix path", "/tmp/test.txt", "/files/data.txt", "/files/data.txt"),
      ("relative URL", "http://localhost/test.txt", "bar.txt", "http://localhost/bar.txt"),
      ("relative path with ..", "/tmp/foo/bar.txt", "../baz/qux.txt", "file:///tmp/baz/qux.txt")
    )

  "create" - {
    forAll(fromPathCases) { (desc, input, output) =>
      s"handles $desc" in {
        val sut = ScriptURL.create(input)
        sut.toString should be (output)
      }
    }

    "accepts an URL" in {
      val url = new URL("http://localhost/test.txt")
      val sut = ScriptURL.create(url)
      sut.toString should be ("http://localhost/test.txt")
    }

    "rejects a relative file path with .." in {
      assertThrows[IllegalArgumentException](ScriptURL.create("../im/relative"))
    }
  }

  "resolve" - {

    forAll(resolveCases) { (desc, original, input, output) =>
      s"handles $desc" in {
        val sut = ScriptURL.create(original)
        val sut2 = sut.resolve(input)
        sut2 should be (ScriptURL.create(output))
      }
    }
  }

  "toFile" - {
    "returns a file with an appropriate path" in {
      val original = "c:\\temp\\test.txt"
      val sut = ScriptURL.create(original)
      val f = sut.toFile

      // Cannot test path accurately on both Windows and Unix, so do a round-trip.
      val sut2 = ScriptURL.create(f.getAbsolutePath)
      sut2.toString should be ("file:///c:/temp/test.txt")
    }
  }

  "isFile" - {
    "returns true for a file URL" in {
      val sut = ScriptURL.create("/tmp/test.txt")
      sut.isFile should be (true)
    }

    "returns false for a non-file URL" in {
      val sut = ScriptURL.create("http://localhost/test.txt")
      sut.isFile should be (false)
    }
  }

  "FileUrl (unapply)" - {
    "recognizes a file URL" in {
      val sut = ScriptURL.create("/tmp/test.txt")
      sut match {
        case FileScriptURL(f) => f.getPath() should include ("test.txt")
        case _ => fail("not a file URL")
      }
    }

    "rejects a non-file URL" in {
      val sut = ScriptURL.create("http://localhost/test.txt")
      sut match {
        case FileScriptURL(f) => fail(s"Non-file URL resulted in $f")
        case _ => // ok
      }
    }
  }

  if (isCaseInsensitiveFileSystem) {
    "equality and hash code" - {
      "is case insensitive (at least on Windows)" in {
        val url1 = ScriptURL.create("file:///C:/path/to/script.js")
        val url2 = ScriptURL.create("file:///c:/path/to/script.js")
        url2 should be(url1)
      }

      "computes hash code without considering case (at least on Windows)" in {
        val url1 = ScriptURL.create("file:///C:/path/to/script.js")
        val url2 = ScriptURL.create("file:///c:/path/to/script.js")
        url2.hashCode() should be(url1.hashCode())
      }

    }
  }
}
