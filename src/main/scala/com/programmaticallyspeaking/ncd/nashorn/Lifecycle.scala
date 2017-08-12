package com.programmaticallyspeaking.ncd.nashorn

/**
  * Enum for the lifecycle of an object in the remote VM.
  */
object Lifecycle {
  sealed trait EnumVal

  /**
    * Lifecycle is when the remote VM is paused.
    */
  case object Paused extends EnumVal

  /**
    * Lifecycle is for the entire debugging session.
    */
  case object Session extends EnumVal
}
