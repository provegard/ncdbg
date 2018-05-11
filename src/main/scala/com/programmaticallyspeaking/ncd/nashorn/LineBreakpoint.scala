package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.sun.jdi.event.BreakpointEvent
import com.sun.jdi.request.{BreakpointRequest, EventRequestManager}

import scala.collection.mutable.ListBuffer

/**
  * A line breakpoint may map to one or more breakable locations, since we cannot distinguish between location
  * column numbers AND because a single script may consist of multiple classes.
  *
  * @param id the breakpoint ID
  * @param breakableLocations the breakable locations
  * @param condition optional condition that must evaluate to truthy for the breakpoint to be hit
  * @param scriptIdentity the identity of the script the breakpoint belongs to
  * @param scriptLocation the line and optional column of the breakpoint
  * @param oneOff whether this is a one-off breakpoint or not
  */
class LineBreakpoint(val id: String, breakableLocations: Seq[BreakableLocation], val condition: Option[String],
                     scriptIdentity: ScriptIdentity, scriptLocation: ScriptLocation, oneOff: Boolean) {

  private object lock
  private val allLocations = ListBuffer[BreakableLocation]()
  private val breakpointRequests = ListBuffer[BreakpointRequest]()

  addBreakableLocations(breakableLocations)

  def isOneOff: Boolean = oneOff

  def belongsTo(script: Script): Boolean = scriptIdentity.matchesScript(script)

  def isUnresolved: Boolean = allLocations.isEmpty

  def addBreakableLocations(locations: Seq[BreakableLocation]): Unit = lock.synchronized {
    locations.foreach { bl =>
      allLocations += bl
      val req = bl.createBreakpointRequest()
      if (oneOff) {
        req.addCountFilter(1)
      }
      LineBreakpoint.associateWithBreakpoint(req, this)
      req.enable()
      breakpointRequests += req
    }
  }

  def toBreakpoint: Breakpoint = {
    // There may be multiple breakable locations for a line (each with its own Location), but to DevTools we
    // only report unique locations.
    Breakpoint(id, breakableLocations.map(bl => LocationInScript(bl.script.id, bl.scriptLocation)).distinct)
  }

  def remove(): Unit = lock.synchronized {
    breakpointRequests.foreach { req =>
        //TODO: Fix ugly
     req.virtualMachine().eventRequestManager().deleteEventRequest(req)
    }
  }
  def contains(breakableLocation: BreakableLocation): Boolean = breakableLocations.contains(breakableLocation)

  def oughtToContain(breakableLocation: BreakableLocation): Boolean = {
    scriptIdentity.matchesScript(breakableLocation.script) &&
      breakableLocation.scriptLocation.lineNumber1Based == scriptLocation.lineNumber1Based
  }
}

object LineBreakpoint {
  def associateWithBreakpoint(br: BreakpointRequest, ab: LineBreakpoint): Unit = {
    br.putProperty("__breakpoint_id", ab.id)
  }

  def getBreakpointId(be: BreakpointEvent): Option[String] = {
    Option(be.request().getProperty("__breakpoint_id")).map(_.toString)
  }
}
