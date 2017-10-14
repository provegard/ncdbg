package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.infra.IdGenerator
import org.slf4s.Logging

import scala.collection.concurrent.TrieMap

class ActiveBreakpoints extends Logging {
  private val breakpointIdGenerator = new IdGenerator("ndb")
  private val enabledBreakpoints = TrieMap[String, ActiveBreakpoint]()

  def activeFor(bl: BreakableLocation): ActiveBreakpoint = {
    enabledBreakpoints.values.find(_.contains(bl)) match {
      case Some(ab) => ab // existing active breakpoint
      case None => ActiveBreakpoint(breakpointIdGenerator.next, Seq(bl), None) // temporary breakpoint (e.g. debugger statement)
    }
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
