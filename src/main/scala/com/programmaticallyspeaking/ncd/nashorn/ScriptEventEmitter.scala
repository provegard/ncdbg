package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ScriptEvent

trait ScriptEventEmitter {
  def emit(event: ScriptEvent): Unit
}
