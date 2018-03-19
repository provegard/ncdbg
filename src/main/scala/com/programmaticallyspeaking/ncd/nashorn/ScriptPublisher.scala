package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Script, ScriptAdded}
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import org.slf4s.Logging

object ScriptPublisher {
  /** This marker if present in a script prevents ScriptPublisher from emitting a ScriptAdded event.
    * This is because we don't want to emit such events for non-persisted compiled scripts when we
    * reconnect to the debug target.
    */
  val PreventPublishingMarker = "__ce3911cf409c4ede90e052fe85486f6d__"
}

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

      val suppressScriptAdded = script.contents.contains(ScriptPublisher.PreventPublishingMarker)

      // Publish the internal event first to ensure that an observer like compileScript sees
      // the script before a dmoain actor observer.
      eventEmitter.emit(NashornDebuggerHost.InternalScriptAdded(script))
      if (!suppressScriptAdded) {
        eventEmitter.emit(ScriptAdded(script))
      }
    }
  }

}
