package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.programmaticallyspeaking.ncd.messaging.{Observable, SerializedSubject}
import org.slf4s.Logging

import scala.collection.concurrent.TrieMap

class LineBreakpoints extends Logging {
  private val breakpointIdGenerator = new IdGenerator("ndb")
  private val byId = TrieMap[String, LineBreakpoint]()
  private val resolvedSubject = new SerializedSubject[BreakpointResolved]()

  def resolvedBreakpoints: Observable[BreakpointResolved] = resolvedSubject

  def addBreakableLocations(script: Script, newLocations: Seq[BreakableLocation]): Unit = {
    // Go through active breakpoints that belong to the script
    // For each BL that matches the active breakpoint, add it
    val lineBreakpointsForScript = byId.values.filter(_.belongsTo(script))
    lineBreakpointsForScript.foreach { bp =>
      val toAdd = newLocations.filter(bp.oughtToContain)
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

  def forBreakableLocation(bl: BreakableLocation): Option[LineBreakpoint] = {
    byId.values.find(_.contains(bl))
  }

  def onBreakpointHit(activeBreakpoint: LineBreakpoint): Unit = {
    if (activeBreakpoint.isOneOff) {
      log.trace(s"Removing one-off breakpoint with id ${activeBreakpoint.id}")
      removeBreakpoint(activeBreakpoint)
    }
  }

  def removeAll(): Unit = {
    //TODO: Not very atomic, this
    byId.foreach(e => e._2.remove())
    byId.clear()
  }

  def removeById(id: String): Unit = {
    byId.get(id) match {
      case Some(activeBp) =>
        log.info(s"Removing breakpoint with id $id")
        removeBreakpoint(activeBp)
      case None =>
        log.warn(s"Got request to remove an unknown breakpoint with id $id")
    }
  }

  private def removeBreakpoint(activeBreakpoint: LineBreakpoint): Unit = {
    activeBreakpoint.remove()
    byId -= activeBreakpoint.id
  }

  def create(id: ScriptIdentity, location: ScriptLocation, locations: Seq[BreakableLocation], condition: Option[String], oneOff: Boolean): LineBreakpoint = {
    val activeBp = new LineBreakpoint(breakpointIdGenerator.next, locations, condition, id, location, oneOff)
    byId += (activeBp.id -> activeBp)
    activeBp
  }
}
