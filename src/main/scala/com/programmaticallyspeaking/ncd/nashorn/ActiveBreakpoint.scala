package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Breakpoint, LocationInScript, Script}

/**
  * An active breakpoint may map to one or more breakable locations, since we cannot distinguish between location
  * column numbers.
  *
  * @param id the breakpoint ID
  * @param breakableLocations the breakable locations
  */
case class ActiveBreakpoint(id: String, breakableLocations: Seq[BreakableLocation], condition: Option[String]) {

  def toBreakpoint: Breakpoint = {
    // There may be multiple breakable locations for a line (each with its own Location), but to DevTools we
    // only report unique locations.
    Breakpoint(id, breakableLocations.map(bl => LocationInScript(bl.script.id, bl.scriptLocation)).distinct)
  }

  def disable(): Unit = breakableLocations.foreach(_.disable())
  def enable(): Unit = breakableLocations.foreach(_.enable())

  def contains(breakableLocation: BreakableLocation) = breakableLocations.contains(breakableLocation)
}
