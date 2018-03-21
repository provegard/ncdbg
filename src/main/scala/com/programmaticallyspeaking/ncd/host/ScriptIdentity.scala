package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.infra.ScriptURL

object ScriptIdentity {

  def fromURL(url: String): ScriptIdentity = URLBasedScriptIdentity(url)
  def fromURLRegex(urlRegex: String): ScriptIdentity = URLRegexBasedScriptIdentity(urlRegex)
  def fromURL(url: ScriptURL): ScriptIdentity = URLBasedScriptIdentity(url.toString)
  def fromId(id: String): ScriptIdentity = IdBasedScriptIdentity(id)
  def fromHash(hash: String): ScriptIdentity = HashBasedScriptIdentity(hash)
}

sealed trait ScriptIdentity {
  def matchesScript(s: Script): Boolean
}

case class HashBasedScriptIdentity(hash: String) extends ScriptIdentity {
  override def matchesScript(s: Script): Boolean = s.contentsHash() == hash

  override def toString: String = s"script with contents hash $hash"
}

case class IdBasedScriptIdentity(id: String) extends ScriptIdentity {
  override def matchesScript(s: Script): Boolean = s.id == id

  override def toString: String = s"script with ID $id"
}

case class URLBasedScriptIdentity(url: String) extends ScriptIdentity {
  val scriptURL = ScriptURL.create(url)
  override def matchesScript(s: Script): Boolean = s.url == scriptURL

  override def toString: String = s"script with URL $url"
}

case class URLRegexBasedScriptIdentity(urlRegex: String) extends ScriptIdentity {
  private val _compiled = urlRegex.r
  override def matchesScript(s: Script): Boolean =
    _compiled.pattern.matcher(s.url.toString).matches()

  override def toString: String = s"script with URL matching $urlRegex"
}
