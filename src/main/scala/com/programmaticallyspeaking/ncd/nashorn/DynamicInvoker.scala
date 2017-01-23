package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi._

import scala.language.implicitConversions
import scala.language.dynamics

class MissingMethodException(val name: String, message: String) extends RuntimeException(message)

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

  protected def findMethod(referenceType: ReferenceType, methodName: String, argCount: Int): Either[Seq[Method], Method] = {
    val sigIndex = methodName.indexOf('(')
    val (name, sig) = if (sigIndex >= 0) {
      // Method name contains signature
      methodName.splitAt(sigIndex)
    } else {
      (methodName, null)
    }
    val namedMethods = referenceType.methodsByName(name).asScala
    namedMethods.find(m => m.argumentTypes().size() == argCount && (sig == null || sig == m.signature())) match {
      case Some(method) => Right(method)
      case None => Left(namedMethods)
    }
  }

  protected def rejectCall[R](methodName: String, referenceType: ReferenceType, args: Seq[Any], candidates: Seq[Method]): R = {
    throw new MissingMethodException(methodName, s"Cannot find a method '$methodName' on type '${referenceType.name()}' matching: " + args.mkString(", ") +
      "\n\nCandidates:\n" + candidates.map(m => s"- ${m.name()}${m.signature()}").mkString("\n"))
  }
}

class DynamicInvoker(thread: ThreadReference, objectReference: ObjectReference) extends Invoker(thread) with Dynamic {
  import scala.collection.JavaConverters._
  import VirtualMachineExtensions._

  def applyDynamic(methodName: String)(args: Any*): Value = {
    findMethod(objectReference.referenceType(), methodName, args.size) match {
      case Right(method) =>
        val argValues = args.map(toValue)
        thread.virtualMachine().withoutClassPrepareRequests {
          objectReference.invokeMethod(suspendedThread(), method, argValues.asJava, ObjectReference.INVOKE_SINGLE_THREADED)
        }
      case Left(candidates) =>
        rejectCall(methodName, objectReference.referenceType(), args, candidates)
    }
  }
}

class StaticInvoker(thread: ThreadReference, classType: ClassType) extends Invoker(thread) with Dynamic {
  import scala.collection.JavaConverters._
  import VirtualMachineExtensions._

  def applyDynamic(methodName: String)(args: Any*): Value = {
    findMethod(classType, methodName, args.size) match {
      case Right(method) =>
        val argValues = args.map(toValue)
        thread.virtualMachine().withoutClassPrepareRequests {
          classType.invokeMethod(suspendedThread(), method, argValues.asJava, ObjectReference.INVOKE_SINGLE_THREADED)
        }
      case Left(candidates) =>
        rejectCall(methodName, classType, args, candidates)
    }
  }
}