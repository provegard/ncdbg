package com.programmaticallyspeaking.ncd.infra

object ErrorUtils {
  case class TypeAndMessage(typ: String, message: String)

  def parseMessage(msg: String): TypeAndMessage = {
    val colonIdx = msg.indexOf(':')
    val tup = if (colonIdx >= 0) {
      val parts = msg.splitAt(colonIdx)
      (parts._1, parts._2.substring(1).trim)
    } else {
      ("Error", msg)
    }
    TypeAndMessage(tup._1, tup._2)
  }
}
