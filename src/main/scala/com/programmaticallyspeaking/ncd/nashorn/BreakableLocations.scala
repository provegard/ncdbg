package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{IdBasedScriptIdentity, Script, ScriptIdentity}
import com.sun.jdi.Location
import org.slf4s.Logging

import scala.collection.concurrent.TrieMap

class BreakableLocations(scripts: Scripts) extends Logging {

  import JDIExtensions._

  private val breakableLocationsByScriptUrl = TrieMap[String, Seq[BreakableLocation]]()

  def add(script: Script, locations: Seq[Location]): Seq[BreakableLocation] = {
    add0(script, gatherBreakableLocations(script, locations))
  }

  private def add0(script: Script, breakableLocations: Seq[BreakableLocation]): Seq[BreakableLocation] = {
    val existing = breakableLocationsByScriptUrl.getOrElse(script.url.toString, Seq.empty)

    val newList = existing ++ breakableLocations
    breakableLocationsByScriptUrl += script.url.toString -> newList
    breakableLocations
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
    val bls = atLine(id, location.lineNumber())
    bls.find(_.hasLocation(location)).orElse(bls.find(_.sameMethodAndLineAs(location)))
  }

  /**
    * Called indirectly when the user sets a breakpoint in DevTools.
    *
    * @param id script identity
    * @param lineNumber line number for the breakpoint
    */
  def atLine(id: ScriptIdentity, lineNumber: Int): Seq[BreakableLocation] = findScriptUrl(id) match {
    case Some(scriptUrl) =>
      breakableLocationsByScriptUrl.get(scriptUrl).map { breakableLocations =>
        breakableLocations.filter(_.scriptLocation.lineNumber1Based == lineNumber)
      }.getOrElse(Seq.empty)
    case None =>
      id match {
        case IdBasedScriptIdentity(scriptId) =>
          throw new IllegalArgumentException("Unknown script: " + scriptId)
        case _ =>
          // This is not an error since the URL-based ID may match a future script in which case we will
          // emit a BreakpointResolved event at that time.
          Seq.empty
      }
  }

  private def findScriptUrl(id: ScriptIdentity): Option[String] = scripts.byId(id).map(_.url.toString)

  private def gatherBreakableLocations(script: Script, locations: Seq[Location]): Seq[BreakableLocation] = {
    // Find all potentially breakable lines. Create breakable locations from actual locations. Then create
    // candidates for the potentially breakable lines that weren't covered. Such a line may for example belong to
    // a function that hasn't been executed yet.
    locations.map(l => new BreakableLocation(script, l))
  }

}
