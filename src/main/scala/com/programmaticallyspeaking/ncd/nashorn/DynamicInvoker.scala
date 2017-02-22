package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{ComplexNode, ErrorValue, ValueNode}
import com.sun.jdi._

import scala.language.implicitConversions
import scala.language.dynamics
import scala.util.control.NonFatal

class MissingMethodException(val name: String, message: String) extends RuntimeException(message)

class InvocationFailedException(message: String) extends RuntimeException(message)

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

  protected def unpackError[R](handle: => R): R = {
    try handle catch {
      case ex: InvocationException =>
        try {
          // Try to marshal the exception. We expect Marshaller to register an ErrorValue with extra property 'javaStack'.
          val marshaller = new Marshaller(thread, new ThrowingMappingRegistry)
          marshaller.marshal(ex.exception())
          throw new RuntimeException("Invocation failed", ex)
        } catch {
          case e: InvocationFailedException =>
            throw e
          case NonFatal(t) =>
            val rex = new RuntimeException("Marshalling of exception failed", t)
            rex.addSuppressed(ex)
            throw rex
        }
    }
  }

  class ThrowingMappingRegistry extends MappingRegistry {
    override def register(value: Value, valueNode: ComplexNode, extraProperties: Map[String, ValueNode]): Unit = valueNode match {
      case ErrorValue(data, _, _) =>
        extraProperties.get("javaStack") match {
          case Some(stackWithMessage) =>
            throw new InvocationFailedException("Invocation failed: " + stackWithMessage.asString)
          case None =>
            throw new InvocationFailedException(s"Invocation failed: ${data.name}: ${data.message}")
        }
      case _ => // noop
    }
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
          unpackError(objectReference.invokeMethod(suspendedThread(), method, argValues.asJava, ObjectReference.INVOKE_SINGLE_THREADED))
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
          unpackError(classType.invokeMethod(suspendedThread(), method, argValues.asJava, ObjectReference.INVOKE_SINGLE_THREADED))
        }
      case Left(candidates) =>
        rejectCall(methodName, classType, args, candidates)
    }
  }
}