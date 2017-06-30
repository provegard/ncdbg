package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{StepInto, StepOut, StepOver, StepType}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.StepRequestClassFilter
import com.sun.jdi.ThreadReference
import com.sun.jdi.request.StepRequest
import org.slf4s.Logging

trait StepSupport { self: NashornDebuggerHost with Logging =>
  override def step(stepType: StepType): Unit = pausedData match {
    case Some(pd) =>
      log.info(s"Stepping with type $stepType")

      // Note that we don't issue normal step requests to the remove VM, because a script line != a Java line, so if we
      // were to request step out, for example, we might end up in some method that acts as a script bridge.
      stepType match {
        case StepInto =>
          createEnabledStepIntoRequest(pd.thread)
        case StepOver =>
          createEnabledStepOverRequest(pd.thread, pd.isAtDebuggerStatement)
        case StepOut =>
          createEnabledStepOutRequest(pd.thread, pd.isAtDebuggerStatement)
      }

      resumeWhenPaused()
    case None =>
      throw new IllegalStateException("A breakpoint must be active for stepping to work")
  }

  private def createEnabledStepRequest(thread: ThreadReference, depth: Int, count: Int): Unit = {
    val sr = virtualMachine.eventRequestManager().createStepRequest(thread, StepRequest.STEP_LINE, depth)
    sr.addClassFilter(StepRequestClassFilter)
    if (count > 0) sr.addCountFilter(count)
    sr.enable()
  }

  private def createEnabledStepIntoRequest(thread: ThreadReference): Unit = {
    createEnabledStepRequest(thread, StepRequest.STEP_INTO, -1)
  }

  //TODO: Remove two-way dependency between NDH and this trait
  protected def createEnabledStepOverRequest(thread: ThreadReference, isAtDebuggerStatement: Boolean): Unit = {
    createEnabledStepRequest(thread, StepRequest.STEP_OVER, if (isAtDebuggerStatement) 2 else -1)
  }

  private def createEnabledStepOutRequest(thread: ThreadReference, isAtDebuggerStatement: Boolean): Unit = {
    //TODO: Why 3 and not 2??
    createEnabledStepRequest(thread, StepRequest.STEP_OUT, if (isAtDebuggerStatement) 3 else -1)
  }
}
