package com.programmaticallyspeaking.ncd.host

import com.programmaticallyspeaking.ncd.infra.ScriptURL

trait Script {
  val id: String
  val uri: ScriptURL //TODO: Rename to url also

  val lineCount: Int
  val lastLineLength: Int

  val contents: String

  def contentsHash(): String
}
