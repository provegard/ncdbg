package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.request.EventRequestManager
import com.sun.jdi._
import com.sun.jdi.event.EventQueue

class XVirtualMachine(virtualMachine: VirtualMachine) {
  import scala.collection.JavaConverters._

  private var objectReferencesWithDisabledGC = Seq.empty[ObjectReference]
  private var objectReferencesWithDisabledGCForTheEntireSession = Seq.empty[ObjectReference]

  def inner: VirtualMachine = virtualMachine

  def eventQueue(): EventQueue = virtualMachine.eventQueue()

  def process(): Process = virtualMachine.process()

  def allThreads(): Seq[ThreadReference] = virtualMachine.allThreads().asScala

  def allClasses(): Seq[ReferenceType] = virtualMachine.allClasses().asScala

  def classByName(name: String): Option[ReferenceType] = virtualMachine.classesByName(name).asScala.headOption

  def suspend(): Unit = virtualMachine.suspend()

  def resume(): Unit = virtualMachine.resume()

  def eventRequestManager(): EventRequestManager = virtualMachine.eventRequestManager()

  def enableGarbageCollectionForAllReferences(): Unit = {
    enableGarbageCollectionWhereDisabled()
    val refs = objectReferencesWithDisabledGCForTheEntireSession
    objectReferencesWithDisabledGCForTheEntireSession = Seq.empty
    objectReferencesWithDisabledGC = Seq.empty
    refs.foreach(_.enableCollection())
  }

  def enableGarbageCollectionWhereDisabled(): Unit = {
    val refs = objectReferencesWithDisabledGC
    objectReferencesWithDisabledGC = Seq.empty
    refs.foreach(_.enableCollection())
  }

  def disableGarbageCollectionFor(value: Value, entireSession: Boolean = false): Unit = value match {
    case objRef: ObjectReference =>
      // Disable and track the reference so we can enable when we resume
      objRef.disableCollection()
      if (entireSession) objectReferencesWithDisabledGCForTheEntireSession +:= objRef
      else objectReferencesWithDisabledGC +:= objRef
    case _ =>
  }
}
