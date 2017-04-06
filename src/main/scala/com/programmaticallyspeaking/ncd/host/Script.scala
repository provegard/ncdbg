package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.infra.ScriptURL

trait Script {
  val id: String
  val url: ScriptURL

  val lineCount: Int
  val lastLineLength: Int

  val contents: String

  def contentsHash(): String
}
