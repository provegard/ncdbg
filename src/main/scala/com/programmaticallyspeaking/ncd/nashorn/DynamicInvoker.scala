package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{ComplexNode, ErrorValue, ObjectId, ValueNode}
import com.sun.jdi._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.language.dynamics
import scala.util.control.NonFatal

class MissingMethodException(val name: String, message: String) extends RuntimeException(message)

class InvocationFailedException(message: String, val exceptionReference: ObjectReference) extends RuntimeException(message)

object Invokers {
  val shared: Invokers = new Invokers
}

class ReferenceTypeData(refType: ReferenceType) {
  import scala.collection.JavaConverters._
  private val visibleMethods = refType.visibleMethods().asScala
  val methodsByName = visibleMethods.groupBy(_.name())
  val methodsByNameAndSig = visibleMethods.groupBy(m => (m.name(), m.signature()))
}

class Invokers {

  private val typeData = mutable.Map[ReferenceType, ReferenceTypeData]()

  private def referenceTypeDataFor(referenceType: ReferenceType): ReferenceTypeData = {
    typeData.getOrElseUpdate(referenceType, new ReferenceTypeData(referenceType))
  }

  def getDynamic(objectReference: ObjectReference): DynamicInvoker = {
    new DynamicInvoker(objectReference, referenceTypeDataFor(objectReference.referenceType()))
  }

  def getStatic(classType: ClassType): StaticInvoker = {
    new StaticInvoker(classType, referenceTypeDataFor(classType))
  }
}

abstract class Invoker(referenceTypeData: ReferenceTypeData) {
  import scala.collection.JavaConverters._

  protected def toValue(x: Any)(implicit thread: ThreadReference): Value = x match {
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
    case arr: Array[_] =>
      //TODO: Cache this
      thread.virtualMachine().classesByName("java.lang.Object[]").asScala.headOption match {
        case Some(array: ArrayType) =>
          val arrayRef = array.newInstance(arr.length)
          val values = arr.map(toValue).toList
          // Note: This will fail if a value is primitive, because there's no auto-boxing going on.
          arrayRef.setValues(values.asJava)
          arrayRef
        case Some(other) => throw new IllegalStateException("java.lang.Object[] is not an ArrayType, found: " + other)
        case None => throw new IllegalStateException("java.lang.Object[] wasn't found in the VM")
      }

    case z if z == null => null
    case other => throw new IllegalArgumentException("Don't know how to mirror: " + other)
  }

  protected def findMethod(referenceType: ReferenceType, methodName: String, argCount: Int): Either[Seq[Method], Method] = {
    val sigIndex = methodName.indexOf('(')
    if (sigIndex >= 0) {
      // Method name contains signature
      referenceTypeData.methodsByNameAndSig.get(methodName.splitAt(sigIndex)).flatMap(_.headOption) match {
        case Some(method) => Right(method)
        case None => Left(Seq.empty)
      }
    } else {
      // Only a method name
      referenceTypeData.methodsByName.get(methodName).map(_.filter(_.argumentTypeNames().size() == argCount)).map(_.toList) match {
        case Some(method :: Nil) => Right(method)
        case Some(methods) => Left(methods)
        case None => Left(Seq.empty)
      }
    }
  }

  protected def rejectCall[R](methodName: String, referenceType: ReferenceType, args: Seq[Any], candidates: Seq[Method]): R = {
    throw new MissingMethodException(methodName, s"Cannot find a method '$methodName' on type '${referenceType.name()}' matching: " + args.mkString(", ") +
      "\n\nCandidates:\n" + candidates.map(m => s"- ${m.name()}${m.signature()}").mkString("\n"))
  }

  protected def unpackError[R](handle: => R)(implicit thread: ThreadReference): R = {
    try handle catch {
      case ex: IncompatibleThreadStateException =>
        // IncompatibleThreadStateException typically has no message (null), which results in something like:
        // Error: 'null' for object '{"id":"uid-10135"}
        // Wrapping the exception improves the error message a bit.
        throw new RuntimeException("Incompatible thread state", ex)
      case ex: InvocationException =>
        try {
          // Try to marshal the exception. We expect Marshaller to register an ErrorValue with extra property 'javaStack'.
          val marshaller = new Marshaller(new ThrowingMappingRegistry)
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
      case ErrorValue(data, _, _, _) =>
        val exceptionRef = value.asInstanceOf[ObjectReference]
        extraProperties.get("JavaStack") match {
          case Some(stackWithMessage) =>
            throw new InvocationFailedException(stackWithMessage.asString, exceptionRef)
          case None =>
            throw new InvocationFailedException(s"${data.name}: ${data.message}", exceptionRef)
        }
      case _ => // noop
    }

    override def clear(): Unit = {}

    override def byId(id: ObjectId): Option[NashornDebuggerHost.ObjectDescriptor] = None
  }
}

class DynamicInvoker(objectReference: ObjectReference, referenceTypeData: ReferenceTypeData) extends Invoker(referenceTypeData) with Dynamic {
  import scala.collection.JavaConverters._

  def applyDynamic(methodName: String)(args: Any*)(implicit thread: ThreadReference): Value = {
    findMethod(objectReference.referenceType(), methodName, args.size) match {
      case Right(method) =>
        val argValues = args.map(toValue)
        unpackError(objectReference.invokeMethod(thread, method, argValues.asJava, ObjectReference.INVOKE_SINGLE_THREADED))
      case Left(candidates) =>
        rejectCall(methodName, objectReference.referenceType(), args, candidates)
    }
  }
}

class StaticInvoker(classType: ClassType, referenceTypeData: ReferenceTypeData) extends Invoker(referenceTypeData) with Dynamic {
  import scala.collection.JavaConverters._

  def applyDynamic(methodName: String)(args: Any*)(implicit thread: ThreadReference): Value = {
    findMethod(classType, methodName, args.size) match {
      case Right(method) =>
        val argValues = args.map(toValue)
        unpackError(classType.invokeMethod(thread, method, argValues.asJava, ObjectReference.INVOKE_SINGLE_THREADED))
      case Left(candidates) =>
        rejectCall(methodName, classType, args, candidates)
    }
  }
}