package com.programmaticallyspeaking.ncd.infra

/**
  * TODO
  * @param id
  * @param url
  */
case class CompiledScript(id: String, url: String) {
  def toCodeUrl: String = CompiledScript.prefix + Base64.encodeString(id + "|" + url)
}

object CompiledScript {
  private val prefix = "compiled:/"
  def unapply(s: String): Option[CompiledScript] = {
    if (s.startsWith(prefix)) {
      val decoded = Base64.decodeString(s.substring(prefix.length))
      val splitIdx = decoded.indexOf('|')
      val (id, url) = decoded.splitAt(splitIdx)
      Some(CompiledScript(id, url.substring(1)))
    } else None
  }
}