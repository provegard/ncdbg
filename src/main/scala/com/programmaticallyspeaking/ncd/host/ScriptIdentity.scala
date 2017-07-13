package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.infra.ScriptURL

object ScriptIdentity {

  def fromURL(url: String): ScriptIdentity = URLBasedScriptIdentity(url)
  def fromURL(url: ScriptURL): ScriptIdentity = URLBasedScriptIdentity(url.toString)
  def fromId(id: String): ScriptIdentity = IdBasedScriptIdentity(id)
}

sealed trait ScriptIdentity

case class IdBasedScriptIdentity(id: String) extends ScriptIdentity

case class URLBasedScriptIdentity(url: String) extends ScriptIdentity
