package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.infra.IdGenerator
import org.slf4s.Logging

import scala.collection.concurrent.TrieMap

class ActiveBreakpoints extends Logging {
  private val breakpointIdGenerator = new IdGenerator("ndb")
  private val enabledBreakpoints = TrieMap[String, ActiveBreakpoint]()

  /**
    * Add new breakable locations to an existing breakpoint
    * @param breakpointId the ID of the breakpoint to add to
    * @param newLocations locations to add
    */
  def addBreakableLocations(breakpointId: String, newLocations: Seq[BreakableLocation]) = {
    if (newLocations.nonEmpty) {
      enabledBreakpoints.get(breakpointId) match {
        case Some(breakpoint) =>
          log.info(s"Adding breakable locations to breakpoint $breakpointId for script ${newLocations.head.script.id}")
          // Replace the breakpoint with a new one
          val newBp = breakpoint.copy(breakableLocations = breakpoint.breakableLocations ++ newLocations)
          newBp.enable() // make sure all the new locations are enabled
          enabledBreakpoints.put(breakpointId, newBp)
        case None =>
          log.warn(s"Unexpected, cannot add breakable locations to unknown breakpoint $breakpointId")
      }
    }
  }

  def activeFor(bl: BreakableLocation): Option[ActiveBreakpoint] = {
    enabledBreakpoints.values.find(_.contains(bl))
  }

  def disableAll(): Unit = {
    //TODO: Not very atomic, this
    enabledBreakpoints.foreach(e => e._2.disable())
    enabledBreakpoints.clear()
  }

  def disableById(id: String): Unit = {
    enabledBreakpoints.get(id) match {
      case Some(activeBp) =>
        log.info(s"Removing breakpoint with id $id")
        activeBp.disable()
        enabledBreakpoints -= activeBp.id
      case None =>
        log.warn(s"Got request to remove an unknown breakpoint with id $id")
    }
  }

  def create(locations: Seq[BreakableLocation], condition: Option[String]): ActiveBreakpoint = {
    val activeBp = ActiveBreakpoint(breakpointIdGenerator.next, locations, condition)
    activeBp.enable()
    enabledBreakpoints += (activeBp.id -> activeBp)
    activeBp
  }
}
