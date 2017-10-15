package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded}
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import org.slf4s.Logging

class ScriptPublisher(eventEmitter: ScriptEventEmitter) extends Logging {

  private var _publishedScriptUrls = Set[ScriptURL]()

  def publish(script: Script): Unit = {
    // Try to ensure that only the first thread observes "isKnownScript" to be true for a particular URL
    val old = _publishedScriptUrls
    _publishedScriptUrls += script.url
    val isKnownScript = old.contains(script.url)

    if (isKnownScript) {
      log.debug(s"Script with URI '${script.url}' is already known")
    } else {
      // Reason for logging double at different levels: info typically goes to the console, debug to the log file.
      log.debug(s"Adding script with ID '${script.id}', URI '${script.url}' and hash '${script.contentsHash()}'")
      log.info(s"Adding script with URI '${script.url}'")
      eventEmitter.emit(ScriptAdded(script))
    }
  }

}
