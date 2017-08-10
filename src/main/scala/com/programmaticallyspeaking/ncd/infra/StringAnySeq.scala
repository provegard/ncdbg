package com.programmaticallyspeaking.ncd.infra

/**
  * Extracts a `Seq[(String, Any)]` in pattern matching. For example:
  *
  * {{{
  *   someValue match {
  *     case StringAnySeq(theSeq) => /* operate on theSeq */
  *   }
  * }}}
  *
  * Note that an empty sequence will always match, since it's not possible to determine the runtime type of keys in an
  * empty sequence.
  */
object StringAnySeq {
  def unapply(some: Any): Option[Seq[(String, Any)]] = some match {
    case aSeq: Seq[_] =>
      // Otherwise, check that it contains a string key
      aSeq.headOption match {
        case Some((key: String, _)) => Some(aSeq.asInstanceOf[Seq[(String, Any)]]) // seq with string key qualifies
        case None => Some(aSeq.asInstanceOf[Seq[(String, Any)]]) // empty seq qualifies
        case _ => None // non-empty, non-string-key
      }

    case _ => None
  }
}