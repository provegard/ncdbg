package com.programmaticallyspeaking.ncd.infra

import java.security.MessageDigest

object Hasher {
  private val md5 = MessageDigest.getInstance("md5")

  def md5(data: Array[Byte]): String = {
    val buf = md5.digest(data)
    buf.map("%02X".format(_)).mkString
  }
}