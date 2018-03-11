package com.programmaticallyspeaking.ncd.infra

import java.nio.charset.StandardCharsets

object Base64 {
  private val encoder = java.util.Base64.getEncoder
  private val decoder = java.util.Base64.getDecoder

  def encodeString(s: String): String = {
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    encoder.encodeToString(bytes)
  }

  def decodeString(s: String): String = {
    val bytes = decoder.decode(s)
    new String(bytes, StandardCharsets.UTF_8)
  }
}
