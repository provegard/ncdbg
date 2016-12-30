package com.programmaticallyspeaking.ncd.host

trait Script {
  val id: String
  val uri: String

  val lineCount: Int
  val lastLineLength: Int

  val contents: String

  def contentsHash(): String
}
