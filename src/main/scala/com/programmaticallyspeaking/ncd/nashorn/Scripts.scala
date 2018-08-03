package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.{IdGenerator, PathUtils, ScriptURL}

import scala.collection.concurrent.TrieMap

case class SuggestResult(script: Script, replaced: Option[Script])

class Scripts {
  private val _scriptByUrl = TrieMap[ScriptURL, Script]()
  private val _scriptById = TrieMap[String, Script]()

  def scripts: Seq[Script] = _scriptById.values.toSeq

  private def isAnonymousScript(s: Script) = s.url.toString == ""

  def suggest(script: Script): Option[SuggestResult] = {
    require(byId(ScriptIdentity.fromId(script.id)).isEmpty, s"Script with ID ${script.id} has already been added")

    // No URL checking for an anonymous script
    if (isAnonymousScript(script)) {
      _scriptById += script.id -> script
      return Some(SuggestResult(script, None))
    }

    // For a recompilation, we will (most likely) already have the original script that was recompiled (recompilation
    // happens for example when a function inside the eval script is called with known types). We find the original
    // script by comparing contents hashes. If we find the original script, we just discard the new one and use the
    // original.
    _scriptByUrl.values.find(_.contentsHash() == script.contentsHash()) match {
      case Some(scriptWithSameSource) =>
        // Note that we add a map entry for the original script with the new URL as key. This way we'll find our
        // reused script using all its "alias URLs".
        // Note 2: I worry that comparing contents hashes isn't enough - that we need to verify no overlapping
        // line locations also. But we don't have locations here, and I don't want to do too much defensive coding.
        _scriptByUrl += script.url -> scriptWithSameSource
        // Don't add an entry to _scriptById since we ignore the new script
        Some(SuggestResult(scriptWithSameSource, None))
      case None =>
        _scriptByUrl.get(script.url) match {
          case Some(_) if !script.version.main =>
            // Probably a recompilation of a script that we already have replaced. Ignore!
            None
          case Some(existing) =>
            // This is a new script with new contents but with the same URL as an old one - it is likely a script
            // that has been reloaded via Nashorn's 'load' function.
            // Supporting debugging of a replaced script is certainly possible but it becomes complex especially
            // with source maps. It is likely a rare edge case, so we just replace the old script with the new
            // one (and the caller will have to do some breakpoint bookkeeping).

            // Which is newest? Check version.
            versionOrder(existing, script) match {
              case (oldest, _) if oldest eq script =>
                // The new script is the oldest, just ignore it!
                None
              case _ =>
                // The new script is newer than the existing
                _scriptById -= existing.id
                _scriptByUrl += script.url -> script
                _scriptById += script.id -> script
                Some(SuggestResult(script, Some(existing)))
            }
          case None =>
            // New script wih unique URL
            _scriptByUrl += script.url -> script
            _scriptById += script.id -> script
            Some(SuggestResult(script, None))
        }
    }
  }

  private def versionOrder(s1: Script, s2: Script): (Script, Script) = {
    if (s1.version.version < s2.version.version) (s1, s2)
    else (s2, s1)
  }

  def byId(id: ScriptIdentity): Option[Script] = {
    id match {
      case i: IdBasedScriptIdentity => _scriptById.get(i.id)
      case u: URLBasedScriptIdentity => _scriptByUrl.get(u.scriptURL)
      case other => scripts.find(other.matchesScript)
    }
  }
}
