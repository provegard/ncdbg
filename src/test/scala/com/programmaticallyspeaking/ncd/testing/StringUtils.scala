package com.programmaticallyspeaking.ncd.testing

import java.nio.charset.Charset
import java.util.Base64

object StringUtils {
  private val encoder = Base64.getEncoder
  private val decoder = Base64.getDecoder
  private val utf8 = Charset.forName("utf-8")

  def toBase64(s: String): String = encoder.encodeToString(s.getBytes(utf8))
  def fromBase64(s: String): String = new String(decoder.decode(s), utf8)
}
