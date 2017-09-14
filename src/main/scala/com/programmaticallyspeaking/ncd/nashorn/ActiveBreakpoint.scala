package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.Breakpoint

/**
  * An active breakpoint may map to one or more breakable locations, since we cannot distinguish between location
  * column numbers.
  *
  * @param id the breakpoint ID
  * @param breakableLocations the breakable locations
  */
case class ActiveBreakpoint(id: String, breakableLocations: Seq[BreakableLocation], condition: Option[String]) {
  assert(breakableLocations.nonEmpty, "An active breakpoint needs at least one breakable location")

  val firstBreakableLocation = breakableLocations.head

  def toBreakpoint: Breakpoint = {
    val script = firstBreakableLocation.script
    // There may be multiple breakable locations for a line (each with its own Location), but to DevTools we
    // only report unique locations.
    Breakpoint(id, script.id, Some(script.url), breakableLocations.map(_.scriptLocation).distinct)
  }

  def disable(): Unit = breakableLocations.foreach(_.disable())
  def enable(): Unit = breakableLocations.foreach(_.enable())

  def contains(breakableLocation: BreakableLocation) = breakableLocations.contains(breakableLocation)
}
