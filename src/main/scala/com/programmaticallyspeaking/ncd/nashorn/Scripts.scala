package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{IdBasedScriptIdentity, Script, ScriptIdentity, URLBasedScriptIdentity}
import com.programmaticallyspeaking.ncd.infra.{IdGenerator, PathUtils, ScriptURL}

import scala.collection.concurrent.TrieMap

class Scripts {
  private val scriptIdGenerator = new IdGenerator("ndx") // special prefix for replacement scripts

  private val _scripts = TrieMap[ScriptURL, Script]()

  def scripts: Seq[Script] = _scripts.values.groupBy(_.id).flatMap(_._2.headOption).toSeq

  def suggest(script: Script): Script = {
    require(byId(ScriptIdentity.fromId(script.id)).isEmpty, s"Script with ID ${script.id} has already been added")
    // For a recompilation, we will (most likely) already have the original script that was recompiled (recompilation
    // happens for example when a function inside the eval script is called with known types). We find the original
    // script by comparing contents hashes. If we find the original script, we just discard the new one and use the
    // original.
    _scripts.values.find(_.contentsHash() == script.contentsHash()) match {
      case Some(scriptWithSameSource) =>
        // Note that we add a map entry for the original script with the new URL as key. This way we'll find our
        // reused script using all its "alias URLs".
        // Note 2: I worry that comparing contents hashes isn't enough - that we need to verify no overlapping
        // line locations also. But we don't have locations here, and I don't want to do too much defensive coding.
        _scripts += script.url -> scriptWithSameSource
        scriptWithSameSource
      case None =>
        _scripts.get(script.url) match {
          case Some(_) =>
            // This is a new script with new contents but with the same URL as an old one - it is likely a script
            // that has been reloaded via Nashorn's 'load' function.
            // The choice of using the ID as suffix is pretty arbitrary - it could also be a sequence number.
            val newId = scriptIdGenerator.next
            //TODO: Don't use PathUtils here, it's URL now
            val newURLString = PathUtils.insertNameSuffix(script.url.toString, "_" + newId)
            val newURL = ScriptURL.create(newURLString)

            val replacement = ScriptImpl.fromSource(newURL, script.contents, newId)
            _scripts += newURL -> replacement
            replacement

          case None =>
            _scripts += script.url -> script
            script
        }
    }
  }

  def byId(id: ScriptIdentity): Option[Script] = {
    id match {
      case IdBasedScriptIdentity(x) =>
        _scripts.values.find(_.id == x)
      case URLBasedScriptIdentity(url) =>
        _scripts.get(ScriptURL.create(url))
    }
  }
}
