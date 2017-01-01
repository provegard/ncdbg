package com.programmaticallyspeaking.ncd.infra

/**
  * Extracts a `Map[String, Any]` in pattern matching. For example:
  *
  * {{{
  *   someValue match {
  *     case StringAnyMap(theMap) => /* operate on theMap */
  *   }
  * }}}
  *
  * Note that an empty map will always match, since it's not possible to determine the runtime type of keys in an
  * empty map.
  */
object StringAnyMap {
  def unapply(some: Any): Option[Map[String, Any]] = some match {
    case aMap: Map[_, _] =>
      // Otherwise, check that it contains a string key
      aMap.headOption match {
        case Some((key: String, _)) => Some(aMap.asInstanceOf[Map[String, Any]]) // map with string key qualifies
        case None => Some(aMap.asInstanceOf[Map[String, Any]]) // empty map qualifies
        case _ => None // non-empty, non-string-key
      }

    case _ => None
  }
}