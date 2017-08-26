package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ValueNode
import com.sun.jdi._

/**
  * Represents an object that doesn't exist in the remote VM.
  */
class LocalObject(val values: Map[String, ValueNode]) extends Value {
  override def `type`(): Type = notSupported
  override def virtualMachine(): VirtualMachine = notSupported

  private def notSupported[R] = throw new UnsupportedOperationException("LocalObject")
}