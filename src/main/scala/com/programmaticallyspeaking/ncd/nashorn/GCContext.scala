package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.{ObjectCollectedException, Value}

class GCContext(vm: XVirtualMachine) {
  def pin[R <: Value](lifecycle: Lifecycle.EnumVal)(f: => R): R = pin(lifecycle, 3)(f)

  private def pin[R <: Value](lifecycle: Lifecycle.EnumVal, attemptsLeft: Int)(f: => R): R = {
    try {
      val v = f
      if (lifecycle != Lifecycle.None) {
        vm.disableGarbageCollectionFor(v, lifecycle == Lifecycle.Session)
      }
      v
    } catch {
      case _: ObjectCollectedException if attemptsLeft > 0 =>
        pin(lifecycle, attemptsLeft - 1)(f)
    }
  }
}
