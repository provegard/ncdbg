package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.ThreadReference

trait ThreadUser {
  val thread: ThreadReference

  protected def suspendedThread(): ThreadReference = {
    require(thread.isSuspended, s"Thread (${thread.name()}) must be suspended.")
    thread
  }
}
