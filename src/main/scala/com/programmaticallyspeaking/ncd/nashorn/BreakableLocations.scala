package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Script, ScriptIdentity}
import com.sun.jdi.{Location, VirtualMachine}
import org.slf4s.Logging

import scala.collection.concurrent.TrieMap

class BreakableLocations(virtualMachine: VirtualMachine, scripts: Scripts) extends Logging {

  import JDIExtensions._

  private val breakableLocationsByScriptUrl = TrieMap[String, Seq[BreakableLocation]]()

  def add(script: Script, locations: Seq[Location]): Unit = {
    add0(script, gatherBreakableLocations(script, locations))
  }

  private def add0(script: Script, breakableLocations: Seq[BreakableLocation]): Unit = {
    val existing = breakableLocationsByScriptUrl.getOrElse(script.url.toString, Seq.empty)

    // Remove existing ones where Location is unset and the line number exists among the new ones. These are placeholders
    // added so that it's possible to set a breakpoint in a function before it has been executed, but now we have real
    // locations for that function (supposedly).
    // TODO: This may be broken if multiple one-liner functions are defined on the same line...
    val lineNumbersOfNewOnes = breakableLocations.map(_.scriptLocation.lineNumber1Based).toSet

    // TODO: Identify BLs no longer relevant due to recompilation. Perhaps by function node ID? If such a BL
    // TODO: is enabled, it needs to be disabled.
    def isObsolete(bl: BreakableLocation) = bl.isPlaceholder && lineNumbersOfNewOnes.contains(bl.scriptLocation.lineNumber1Based)

    val lineNumbersOfEnabled = existing.filter(_.isEnabled).map(_.scriptLocation.lineNumber1Based).toSet

    // Auto-enable a BL if we have an existing one that is enabled for the same line number - regardless of whether
    // it's a placeholder or not. This is untested for non-placeholders, since we probably need a big test script
    // to trigger this case.
    def shouldEnable(bl: BreakableLocation) = lineNumbersOfEnabled.contains(bl.scriptLocation.lineNumber1Based)

    breakableLocations.filter(shouldEnable).foreach { bl =>
      log.debug(s"Auto-enabling breakable location $bl since it's on the same line as a currently enabled one.")
      bl.enable()
    }

    val newList = existing.filterNot(isObsolete) ++ breakableLocations
    breakableLocationsByScriptUrl += script.url.toString -> newList
  }

  def byScriptIdentity(id: ScriptIdentity): Option[Seq[BreakableLocation]] =
    findScriptUrl(id).flatMap(breakableLocationsByScriptUrl.get)

  def byLocation(location: Location): Option[BreakableLocation] = {
    val url = location.scriptURL
    // There may be multiple breakable locations for the same line (even in the same method - e.g. for a 'while'
    // statement that is last in a method). Try to find an exact match first, then fall back to finding a location
    // on the correct line. The fallback is necessary since the passed-in Location may have a code index that is
    // different from the one stored in a BreakableLocation.
    val id = ScriptIdentity.fromURL(url)
    atLine(id, location.lineNumber()).flatMap { bls =>
      bls.find(_.hasLocation(location)).orElse(bls.find(_.sameMethodAndLineAs(location)))
    }
  }

  def atLine(id: ScriptIdentity, lineNumber: Int): Option[Seq[BreakableLocation]] = findScriptUrl(id) match {
    case Some(scriptUrl) =>
      breakableLocationsByScriptUrl.get(scriptUrl).map { breakableLocations =>
        breakableLocations.filter(_.scriptLocation.lineNumber1Based == lineNumber)
      }

    case None =>
      throw new IllegalArgumentException("Unknown script: " + id)
  }

  private def findScriptUrl(id: ScriptIdentity): Option[String] = scripts.byId(id).map(_.url.toString)

  private def potentiallyBreakableLines(script: Script): Seq[Int] = {
    def hasRelevantContent(line: String) = {
      val trimmed = line.trim()
      trimmed != "" && trimmed != "{" && trimmed != "}"
    }
    def looksBreakable(line: Int) = script.sourceLine(line).exists(hasRelevantContent)
    (1 to script.lineCount).filter(looksBreakable)
  }

  private def gatherBreakableLocations(script: Script, locations: Seq[Location]): Seq[BreakableLocation] = {
    val erm = virtualMachine.eventRequestManager()
    // Find all potentially breakable lines. Create breakable locations from actual locations. Then create
    // candidates for the potentially breakable lines that weren't covered. Such a line may for example belong to
    // a function that hasn't been executed yet.
    var lineNumbers = potentiallyBreakableLines(script).toSet
    val breakableLocations = locations.map(l => new BreakableLocation(script, erm, l))
    breakableLocations.foreach(bl => lineNumbers -= bl.scriptLocation.lineNumber1Based)
    breakableLocations ++ lineNumbers.map(line => new BreakableLocation(script, erm, line))
  }

}
