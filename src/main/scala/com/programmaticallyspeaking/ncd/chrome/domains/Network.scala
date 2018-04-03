package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host.ScriptHost

object Network {
  // Network.enable takes parameters (optional), so needs to be a custom class.
  case class enable()

  // Exists to make VSCode happy.
  case class setCacheDisabled(cacheDisabled: Boolean)
}

/**
  * Exists only to satisfy VSCode. Without it, VSCode fails to attach.
  */
class Network(scriptHost: ScriptHost, eventEmitHook: EventEmitHook) extends DomainActor(scriptHost, eventEmitHook) {
  override protected def isEnable = {
    case Network.enable() => // ok
  }

  override protected def handle = {
    case Network.setCacheDisabled(_) => // noop
  }
}
