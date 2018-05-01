package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.request.{BreakpointRequest, EventRequest, EventRequestManager}
import com.sun.jdi._
import com.sun.jdi.event.EventQueue

class XVirtualMachine(virtualMachine: VirtualMachine) {
  import scala.collection.JavaConverters._

  private var objectReferencesWithDisabledGC = Set.empty[ObjectReference]
  private var objectReferencesWithDisabledGCForTheEntireSession = Set.empty[ObjectReference]

  def inner: VirtualMachine = virtualMachine

  def eventQueue(): EventQueue = virtualMachine.eventQueue()

  def process(): Process = virtualMachine.process()

  def allThreads(): Seq[ThreadReference] = virtualMachine.allThreads().asScala

  def allClasses(): Seq[ReferenceType] = virtualMachine.allClasses().asScala

  def classByName(name: String): Option[ReferenceType] = virtualMachine.classesByName(name).asScala.headOption

  def suspend(): Unit = virtualMachine.suspend()

  def resume(): Unit = virtualMachine.resume()

  def eventRequestManager(): EventRequestManager = virtualMachine.eventRequestManager()

  /**
    * Enable GC for all object references, including those that should be kept alive during the entire
    * session.
    */
  def enableGarbageCollectionForAllReferences(): Unit = {
    enableGarbageCollectionWhereDisabled()
    val refs = objectReferencesWithDisabledGCForTheEntireSession
    objectReferencesWithDisabledGCForTheEntireSession = Set.empty
    refs.foreach(_.enableCollection())
  }

  /**
    * Enable GC for object reference for which GC is disabled while the debugger is paused.
    * GC is not enabled for references that should be kept alive during the entire session.
    */
  def enableGarbageCollectionWhereDisabled(): Unit = {
    val refs = objectReferencesWithDisabledGC
    objectReferencesWithDisabledGC = Set.empty
    refs.foreach(_.enableCollection())
  }

  def disableGarbageCollectionFor(value: Value, entireSession: Boolean = false): Unit = value match {
    case objRef: ObjectReference if !objectReferencesWithDisabledGCForTheEntireSession.contains(objRef) =>
      val gcAlreadyDisabled = objectReferencesWithDisabledGC.contains(objRef)
      val shouldPromoteToSessionList = gcAlreadyDisabled && entireSession

      if (!gcAlreadyDisabled) {
        // Disable and track the reference so we can enable when we resume
        objRef.disableCollection()
      }

      if (shouldPromoteToSessionList) {
        // The reference exists in the paused list. Move it to the session list so that it survives longer.
        objectReferencesWithDisabledGC -= objRef
      }

      if (entireSession) objectReferencesWithDisabledGCForTheEntireSession += objRef
      else objectReferencesWithDisabledGC += objRef
    case _ =>
  }

  private def isDeleted(r: EventRequest) = r.toString.contains("(deleted)")

  // Disables enabled requests and returns a restore function that will re-enable them.
  def disableEnabledRequests(): () => Unit = {
    val erm = eventRequestManager()
    val allRequests = erm.breakpointRequests().asScala ++
        erm.methodEntryRequests().asScala ++
        erm.methodExitRequests().asScala ++
        erm.exceptionRequests().asScala ++
        erm.classPrepareRequests().asScala
    val enabledRequests = allRequests.filter(_.isEnabled).toList
    enabledRequests.foreach(_.disable())
    () => enabledRequests.filterNot(isDeleted).foreach(_.enable())
  }

  lazy val version: VMVersion = VMVersion.parse(virtualMachine.version())
}

case class VMVersion(major: Int, minor: Option[Int], patch: Option[Int], build: Option[Int]) {
  def knownBugs: Seq[KnownBug.EnumVal] = KnownBug.all.filter(_.appearsIn(this))
}

object VMVersion {
  /**
    * Parses a version string obtained from `VirtualMachine.version()`.
    *
    * @param version the version string
    * @return a version structure
    */
  def parse(version: String): VMVersion = {
    val parts = version.split(Array('.', '_'))
    if (parts.length > 4) throw new IllegalArgumentException("Unexpected version format: " + version)
    VMVersion(parts.head.toInt,
      parts.lift(1).map(_.toInt),
      parts.lift(2).map(_.toInt),
      parts.lift(3).map(_.toInt))
  }
}

object KnownBug {
  sealed trait EnumVal {
    def appearsIn(v: VMVersion): Boolean
    def bugId: String
    def description: String
  }

  def all: Seq[EnumVal] = Seq(JDK_8187143_Frame_Restart_Crash, JDK_8179072_Abstract_Method_Error)

  /**
    * https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8187143
    * Bug happens after 1.8.0_144
    * Doesn't seem to be fixed in Java 9 or 10
    */
  case object JDK_8187143_Frame_Restart_Crash extends EnumVal {
    override def appearsIn(v: VMVersion): Boolean = {
      v.major >= 9 ||
        v.major == 1 && v.minor.contains(8) && (v.patch.exists(_ > 0) || v.build.exists(_ > 144))
    }

    override def bugId: String = "8187143"

    override def description: String = "Remote VM may terminate on frame restart."
  }

  /**
    * https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8179072
    * Should be fixed in 1.8.0_132
    */
  case object JDK_8179072_Abstract_Method_Error extends EnumVal {
    override def appearsIn(v: VMVersion): Boolean = {
      v.major == 1 && v.minor.contains(8) && v.patch.contains(0) && v.build.exists(_ < 132)
    }

    override def bugId: String = "8179072"

    override def description: String = "Remote VM thread may crash on resume after frame restart."
  }
}