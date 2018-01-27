package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.infra.ScriptURL

object ScriptIdentity {

  def fromURL(url: String): ScriptIdentity = URLBasedScriptIdentity(url)
  def fromURLRegex(urlRegex: String): ScriptIdentity = URLRegexBasedScriptIdentity(urlRegex)
  def fromURL(url: ScriptURL): ScriptIdentity = URLBasedScriptIdentity(url.toString)
  def fromId(id: String): ScriptIdentity = IdBasedScriptIdentity(id)
}

sealed trait ScriptIdentity {
  def matchesScript(s: Script): Boolean
}

case class IdBasedScriptIdentity(id: String) extends ScriptIdentity {
  override def matchesScript(s: Script): Boolean = s.id == id
}

case class URLBasedScriptIdentity(url: String) extends ScriptIdentity {
  val scriptURL = ScriptURL.create(url)
  override def matchesScript(s: Script): Boolean = s.url == scriptURL
}

case class URLRegexBasedScriptIdentity(urlRegex: String) extends ScriptIdentity {
  private val _compiled = urlRegex.r
  override def matchesScript(s: Script): Boolean =
    _compiled.pattern.matcher(s.url.toString).matches()
}
