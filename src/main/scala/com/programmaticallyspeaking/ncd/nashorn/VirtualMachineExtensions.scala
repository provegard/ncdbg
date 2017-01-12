package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.VirtualMachine

import scala.language.implicitConversions

object VirtualMachineExtensions {
  import scala.collection.JavaConverters._

  implicit def virtualMachine2ExtVirtualMachine(virtualMachine: VirtualMachine): ExtVirtualMachine =
    new ExtVirtualMachine(virtualMachine)

  class ExtVirtualMachine(virtualMachine: VirtualMachine) {
    /**
      * Executes a function while all class-prepare requests are disabled. This is important as otherwise loading a class
      * required for method invocation may lead to a deadlock.
      */
    def withoutClassPrepareRequests[R](fun: => R): R = {
      val requests = virtualMachine.eventRequestManager().classPrepareRequests().asScala.filter(_.isEnabled)
      requests.foreach(_.disable())
      try fun finally {
        requests.foreach(_.enable())
      }
    }

  }

}
