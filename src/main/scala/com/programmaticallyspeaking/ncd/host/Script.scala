package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.infra.ScriptURL

trait Script {
  val id: String
  val url: ScriptURL

  val lineCount: Int
  val lastLineLength: Int

  val contents: String

  def sourceLine(lineNumber1Based: Int): Option[String]

  def contentsHash(): String

  def sourceMapUrl(): Option[ScriptURL]

  def sourceUrl(): Option[ScriptURL]
}

// TODO: Should Script be a case class?? A lot simpler...
class ProxyScript(inner: Script) extends Script {
  override val id: String = inner.id
  override val url: ScriptURL = inner.url
  override val lineCount: Int = inner.lineCount
  override val lastLineLength: Int = inner.lastLineLength
  override val contents: String = inner.contents
  override def sourceLine(lineNumber1Based: Int): Option[String] = inner.sourceLine(lineNumber1Based)
  override def contentsHash(): String = inner.contents
  override def sourceMapUrl(): Option[ScriptURL] = inner.sourceMapUrl()
  override def sourceUrl(): Option[ScriptURL] = inner.sourceUrl()
}