package com.programmaticallyspeaking.ncd.testing

object StringUtils {
  def escape(raw: String): String = {
    raw
      .replace("\t", "\\t")
      .replace("\r", "\\r")
      .replace("\n", "\\n")
  }
}
