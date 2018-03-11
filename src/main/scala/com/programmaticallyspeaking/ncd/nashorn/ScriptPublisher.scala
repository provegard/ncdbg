package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded}
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import org.slf4s.Logging

class ScriptPublisher(eventEmitter: ScriptEventEmitter) extends Logging {

  private var _publishedScriptIds = Set[String]()

  def publish(script: Script): Unit = {
    // Try to ensure that only the first thread observes "isKnownScript" to be true for a particular URL
    val old = _publishedScriptIds
    _publishedScriptIds += script.id
    val isKnownScript = old.contains(script.id)

    if (isKnownScript) {
      log.debug(s"Script with ID '${script.id}' is already known")
    } else {
      if (log.underlying.isDebugEnabled)
        log.debug(s"Adding script with ID '${script.id}', URI '${script.url}' and hash '${script.contentsHash()}'")
      else
        log.info(s"Adding script '${script.id}' with URI '${script.url}'")
      eventEmitter.emit(ScriptAdded(script))
    }
  }

}
