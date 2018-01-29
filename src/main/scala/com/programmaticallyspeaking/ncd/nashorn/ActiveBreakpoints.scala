package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.programmaticallyspeaking.ncd.messaging.{Observable, SerializedSubject}
import org.slf4s.Logging

import scala.collection.concurrent.TrieMap

class ActiveBreakpoints extends Logging {
  private val breakpointIdGenerator = new IdGenerator("ndb")
  private val enabledBreakpoints = TrieMap[String, ActiveBreakpoint]()
  private val resolvedSubject = new SerializedSubject[BreakpointResolved]()

  def resolvedBreakpoints: Observable[BreakpointResolved] = resolvedSubject

  def addBreakableLocations(script: Script, newLocations: Seq[BreakableLocation]): Unit = {
    // Go through active breakpoints that belong to the script
    // For each BL that matches the active breakpoint, add it
    val breakpoints = enabledBreakpoints.values.filter(_.belongsTo(script))
    breakpoints.foreach { bp =>
      val toAdd = newLocations.filter(bp.matches)
      if (toAdd.nonEmpty) {
        val willBeResolved = bp.isUnresolved
        log.debug(s"Adding ${toAdd.size} breakable locations to breakpoint ${bp.id}")
        bp.addBreakableLocations(toAdd)
        if (willBeResolved) {
          // Hm, can there be more than one location here?
          val first = toAdd.head
          val item = BreakpointResolved(bp.id, LocationInScript(first.script.id, first.scriptLocation))
          log.info(s"Resolving breakpoint ${bp.id} with location ${first.scriptLocation} in script ${script.id}")
          resolvedSubject.onNext(item)
      }
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

  def create(id: ScriptIdentity, location: ScriptLocation, locations: Seq[BreakableLocation], condition: Option[String]): ActiveBreakpoint = {
    val activeBp = new ActiveBreakpoint(breakpointIdGenerator.next, locations, condition, id, location)
    enabledBreakpoints += (activeBp.id -> activeBp)
    activeBp
  }
}
