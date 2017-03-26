package com.programmaticallyspeaking.ncd.infra

import scala.language.implicitConversions

object BetterOption {

  implicit def option2Better[A](opt: Option[A]): BetterOption[A] = new BetterOption[A](opt)

  class BetterOption[A](opt: Option[A]) {
    def toEither(msg: => String): Either[String, A] = opt match {
      case Some(a) => Right(a)
      case None => Left(msg)
    }

  }
}
