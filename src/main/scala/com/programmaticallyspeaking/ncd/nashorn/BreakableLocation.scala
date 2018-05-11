package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Script, ScriptLocation}
import com.sun.jdi.Location
import com.sun.jdi.request.{BreakpointRequest, EventRequest, EventRequestManager}

object BreakableLocation {
  private def scriptLocationFromScriptAndLocation(script: Script, location: Location): ScriptLocation = {
    ScriptLocation.fromScriptAndLine(script, location.lineNumber())
  }
}

/**
  * Represents a location in a script that the debugger can break at.
  *
  * @param script the script that contains the location
  * @param location the location
  */
class BreakableLocation private(val script: Script, val scriptLocation: ScriptLocation, location: Location) {
  def hasLocation(loc: Location): Boolean = loc == location

  import JDIExtensions._

  def this(script: Script, location: Location) =
    this(script, BreakableLocation.scriptLocationFromScriptAndLocation(script, location), location)

  def sameMethodAndLineAs(l: Location): Boolean = l.sameMethodAndLineAs(Some(location))

  def createBreakpointRequest(): BreakpointRequest = {
    val eventRequestManager = location.virtualMachine().eventRequestManager()
    val breakpointRequest = eventRequestManager.createBreakpointRequest(location)
    breakpointRequest
  }

  override def toString: String = s"${script.id}:${scriptLocation.lineNumber1Based} ($location)"
}
