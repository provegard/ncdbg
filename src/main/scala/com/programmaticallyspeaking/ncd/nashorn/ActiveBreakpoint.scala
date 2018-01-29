package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.sun.jdi.event.BreakpointEvent
import com.sun.jdi.request.{BreakpointRequest, EventRequestManager}

import scala.collection.mutable.ListBuffer

/**
  * An active breakpoint may map to one or more breakable locations, since we cannot distinguish between location
  * column numbers.
  *
  * @param id the breakpoint ID
  * @param breakableLocations the breakable locations
  */
class ActiveBreakpoint(val id: String, breakableLocations: Seq[BreakableLocation], val condition: Option[String],
                       scriptIdentity: ScriptIdentity, scriptLocation: ScriptLocation) {

  private object lock
  private val allLocations = ListBuffer[BreakableLocation]()
  private val breakpointRequests = ListBuffer[BreakpointRequest]()

  addBreakableLocations(breakableLocations)

  def belongsTo(script: Script): Boolean = scriptIdentity.matchesScript(script)

  def isUnresolved: Boolean = allLocations.isEmpty

  def addBreakableLocations(locations: Seq[BreakableLocation]): Unit = lock.synchronized {
    locations.foreach { bl =>
      allLocations += bl
      val req = bl.createBreakpointRequest()
      ActiveBreakpoint.associateWithBreakpoint(req, this)
      req.enable()
      breakpointRequests += req
    }
  }

  def toBreakpoint: Breakpoint = {
    // There may be multiple breakable locations for a line (each with its own Location), but to DevTools we
    // only report unique locations.
    Breakpoint(id, breakableLocations.map(bl => LocationInScript(bl.script.id, bl.scriptLocation)).distinct)
  }

  //TODO: Fix bad name, this is really delete!
  def disable(): Unit = lock.synchronized {
    breakpointRequests.foreach { req =>
        //TODO: Fix ugly
     req.virtualMachine().eventRequestManager().deleteEventRequest(req)
    }
  }
  def contains(breakableLocation: BreakableLocation) = breakableLocations.contains(breakableLocation)

  def matches(breakableLocation: BreakableLocation): Boolean = {
    // TODO: Do we need to check column here? Or can we simply remove column support altogether??
    breakableLocation.scriptLocation.lineNumber1Based == scriptLocation.lineNumber1Based
  }
}

object ActiveBreakpoint {
  def associateWithBreakpoint(br: BreakpointRequest, ab: ActiveBreakpoint): Unit = {
    br.putProperty("__breakpoint_id", ab.id)
  }

  def getBreakpointId(be: BreakpointEvent): Option[String] = {
    Option(be.request().getProperty("__breakpoint_id")).map(_.toString)
  }
}
