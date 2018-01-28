package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Script, ScriptLocation}
import com.sun.jdi.Location
import com.sun.jdi.request.{BreakpointRequest, EventRequest, EventRequestManager}

object BreakableLocation {
  // TODO: Move elsewhere
  def scriptLocationFromScriptAndLocation(script: Script, location: Location): ScriptLocation = {
    scriptLocationFromScriptAndLine(script, location.lineNumber())
  }

  def scriptLocationFromScriptAndLine(script: Script, lineNumber1: Int): ScriptLocation = {
    val lineNo = lineNumber1
    // Use index of first non-whitespace
    val colNo = script.sourceLine(lineNo).map(line => 1 + line.indexWhere(!_.isWhitespace))
    ScriptLocation(lineNo, colNo)
  }
}

/**
  * Represents a location in a script that the debugger can break at.
  *
  * @param script the script that contains the location
  * @param eventRequestManager [[EventRequestManager]] instance for creating/removing a breakpoint
  * @param location the location
  */
class BreakableLocation private(val script: Script, eventRequestManager: EventRequestManager, val scriptLocation: ScriptLocation, location: Location) {
  def hasLocation(loc: Location): Boolean = loc == location

  import JDIExtensions._

  def this(script: Script, eventRequestManager: EventRequestManager, location: Location) =
    this(script, eventRequestManager, BreakableLocation.scriptLocationFromScriptAndLocation(script, location), location)

  def sameMethodAndLineAs(l: Location): Boolean = l.sameMethodAndLineAs(Some(location))

  def createBreakpointRequest(): BreakpointRequest = {
    val breakpointRequest = eventRequestManager.createBreakpointRequest(location)

    // Assume script code runs in a single thread, so pausing that thread should be enough.
    breakpointRequest.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)

    breakpointRequest
  }

  override def toString: String = s"${script.id}/$scriptLocation ($location)"
}
