package com.programmaticallyspeaking.ncd.infra

import java.nio.charset.StandardCharsets

import com.programmaticallyspeaking.ncd.testing.AsyncUnitTest

import scala.concurrent.Future

class HasherTest extends AsyncUnitTest {

  "Hasher.md5" - {
    "hashes a known value" in {
      val bytes = "foobar".getBytes(StandardCharsets.US_ASCII)
      Hasher.md5(bytes) should be ("3858F62230AC3C915F300C664312C63F")
    }

    "hashes multiple times" in {
      "barbar".getBytes(StandardCharsets.US_ASCII)
      val bytes = "barbar".getBytes(StandardCharsets.US_ASCII)
      Hasher.md5(bytes) should be ("5426824942DB4253F87A1009FD5D2D4F")
    }

    "is thread safe" in {
      val bytes = "foobar".getBytes(StandardCharsets.US_ASCII)
      val fs = (1 to 50).map { i => Future { Hasher.md5(bytes) } }
      val all = Future.sequence(fs)
      whenReady(all) { results =>
        results.distinct should be (Seq("3858F62230AC3C915F300C664312C63F"))
      }
    }
  }
}