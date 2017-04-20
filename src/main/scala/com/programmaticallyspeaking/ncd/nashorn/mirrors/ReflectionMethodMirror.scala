package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.host.FunctionNode
import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, Marshaller}
import com.sun.jdi.{ArrayReference, ObjectReference, Value}

class ReflectionMethodMirror(method: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._

  import scala.collection.JavaConverters._

  private lazy val invoker = new DynamicInvoker(marshaller.thread, method)

  lazy val name: String = invoker.getName().asString

  lazy val declaringClassName: String = nameOfClass(invoker.getDeclaringClass())

  lazy val returnTypeName: String = nameOfClass(invoker.getReturnType())

  lazy val parameterTypeNames: Seq[String] = invoker.getParameterTypes() match {
    case arr: ArrayReference => arr.getValues.asScala.map(nameOfClass)
    case other => throw new IllegalStateException("Method.getParameterTypes returned a non-array: " + other)
  }

  private def nameOfClass(c: Value) = c match {
    case ref: ObjectReference => new ClassMirror(ref).name
    case other => throw new IllegalStateException("Unexpected non-objectref (for Class): " + other)
  }

  def asFunctionNode: FunctionNode = marshaller.marshal(method) match {
    case fun: FunctionNode => fun
    case other => throw new IllegalStateException(s"$method didn't marshal to FunctionNode, but to: $other")
  }
}
