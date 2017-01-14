package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi._

import scala.language.implicitConversions
import scala.language.dynamics

abstract class Invoker(val thread: ThreadReference) extends ThreadUser {
  import scala.collection.JavaConverters._

  protected def toValue(x: Any): Value = x match {
    case v: Value => v
    case b: Boolean => thread.virtualMachine().mirrorOf(b)
    case b: Byte => thread.virtualMachine().mirrorOf(b)
    case c: Char => thread.virtualMachine().mirrorOf(c)
    case f: Float => thread.virtualMachine().mirrorOf(f)
    case d: Double => thread.virtualMachine().mirrorOf(d)
    case i: Int => thread.virtualMachine().mirrorOf(i)
    case s: Short => thread.virtualMachine().mirrorOf(s)
    case l: Long => thread.virtualMachine().mirrorOf(l)
    case s: String => thread.virtualMachine().mirrorOf(s)
    case z if z == null => null
    case other => throw new IllegalArgumentException("Don't know how to mirror: " + other)
  }

  protected def findMethod(referenceType: ReferenceType, methodName: String, argCount: Int): Option[Method] = {
    val sigIndex = methodName.indexOf('(')
    val (name, sig) = if (sigIndex >= 0) {
      // Method name contains signature
      methodName.splitAt(sigIndex)
    } else {
      (methodName, null)
    }
    referenceType.methodsByName(name).asScala.find(m => m.argumentTypes().size() == argCount && (sig == null || sig == m.signature()))
  }
}

class DynamicInvoker(thread: ThreadReference, objectReference: ObjectReference) extends Invoker(thread) with Dynamic {
  import scala.collection.JavaConverters._

  def applyDynamic(methodName: String)(args: Any*): Value = {
    findMethod(objectReference.referenceType(), methodName, args.size) match {
      case Some(method) =>
        val argValues = args.map(toValue)
        objectReference.invokeMethod(suspendedThread(), method, argValues.asJava, ObjectReference.INVOKE_SINGLE_THREADED)
      case None =>
        throw new IllegalArgumentException(s"Cannot find a method '$methodName' on type '${objectReference.referenceType().name()}' matching: " + args.mkString(", "))
    }
  }
}

class StaticInvoker(thread: ThreadReference, classType: ClassType) extends Invoker(thread) with Dynamic {
  import scala.collection.JavaConverters._

  def applyDynamic(methodName: String)(args: Any*): Value = {
    findMethod(classType, methodName, args.size) match {
      case Some(method) =>
        val argValues = args.map(toValue)
        classType.invokeMethod(suspendedThread(), method, argValues.asJava, ObjectReference.INVOKE_SINGLE_THREADED)
      case None =>
        throw new IllegalArgumentException(s"Cannot find a method '$methodName' on type '${classType.name()}' matching: " + args.mkString(", "))
    }
  }
}