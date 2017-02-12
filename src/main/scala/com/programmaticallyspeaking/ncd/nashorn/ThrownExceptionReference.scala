package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.{ObjectReference, Type, Value, VirtualMachine}

/** Wrapper for a thrown exception. [[Marshaller]] recognizes this type to mean that the exception reference should be
  * marshalled as something thrown rather than something evaluated. For example, if the user evaluates a caught error
  * inside a catch handler/block, the result should success (error value) rather than failure (error thrown).
  */
class ThrownExceptionReference(vm: VirtualMachine, val exception: ObjectReference) extends Value {
  override def `type`(): Type = exception.`type`()

  override def virtualMachine(): VirtualMachine = vm
}
