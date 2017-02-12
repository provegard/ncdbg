package com.programmaticallyspeaking.ncd.infra

import scala.util.{Success, Try}

object StringUtils {

  def isUnsignedInt(s: String) = s match {
    case UnsignedIntString(_) => true
    case _ => false
  }

  object UnsignedIntString {
    def unapply(s: String): Option[Int] = Try(s.toInt) match {
      case Success(value) if value >= 0 => Some(value)
      case _ => None
    }
  }
}
