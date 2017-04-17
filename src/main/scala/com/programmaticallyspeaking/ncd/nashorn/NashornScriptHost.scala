package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ScriptHost
import com.sun.jdi.VirtualMachine
import com.sun.jdi.event.EventSet

trait NashornScriptOperation
case class NashornEventSet(eventSet: EventSet) extends NashornScriptOperation

/**
  * Extends [[ScriptHost]] by adding methods for activating the host. This is a trait so that we can wrap it using
  * a typed actor and ensure that *all* access happens inside an actor.
  */
trait NashornScriptHost extends ScriptHost {

  // TODO: Should this return a resume flag? Is it ok to resume on another thread?
  def handleOperation(eventQueueItem: NashornScriptOperation): Unit

  def initialize(): Unit

  val virtualMachine: VirtualMachine
}
