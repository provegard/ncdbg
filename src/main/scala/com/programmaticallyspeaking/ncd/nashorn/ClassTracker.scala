package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.{ReferenceType, VirtualMachine}

/** Used for tracking classes added during some operation. Usage:
  * {{{
  * val tracker = new ClassTracker(vm)
  * // do some operation...
  * val added = tracker.addedClasses()
  * }}}
  * @param virtualMachine virtual machine used for listing classes
  */
class ClassTracker(virtualMachine: VirtualMachine) {
  import scala.collection.JavaConverters._

  // Capture known classes when pausing, to be able to diff afterwards. This is currently the only way I know to
  // detect classes added during code evaluation.
  private val classesBefore = virtualMachine.allClasses().asScala.toSet

  def addedClasses(): Seq[ReferenceType] = {
    val classesNow = virtualMachine.allClasses().asScala.toSet
    classesNow.diff(classesBefore).toSeq
  }
}