package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Breakpoint, Script}
import com.sun.jdi.request.BreakpointRequest

class BreakpointRequestWrapper(val script: Script, breakpointRequest: BreakpointRequest, val id: String) {
  private var isEnabledOnce = false

  def enableOnceIfDisabled(): Unit = {
    if (!breakpointRequest.isEnabled) {
      breakpointRequest.addCountFilter(1)
      breakpointRequest.enable()
      isEnabledOnce = true
    }
  }

  def disableIfEnabledOnce(): Unit = {
    if (isEnabledOnce) {
      breakpointRequest.disable()
      isEnabledOnce = false
    }
  }

  def setEnabled(b: Boolean) = breakpointRequest.setEnabled(b)

//  def toBreakpoint: Nothing = new Nothing(id, script.id, breakpointRequest.location.lineNumber)
  def toBreakpoint: Breakpoint = Breakpoint(id, script.id, breakpointRequest.location.lineNumber())

  val lineNumber: Int = breakpointRequest.location.lineNumber
}
