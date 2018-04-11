package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.nashorn.{Invokers, Marshaller}
import com.sun.jdi._

import scala.collection.immutable.Stream.Empty
import scala.language.implicitConversions
import scala.util.{Success, Try}

/**
  * Mirror for `jdk.nashorn.internal.runtime.ScriptObject`.
  *
  * @param scriptObject the `ScriptObject` instance to interact with
  */
class ScriptObjectMirror(val scriptObject: ObjectReference)(implicit marshaller: Marshaller) {
  import Mirrors._
  import ScriptObjectMirror._

  private implicit val thread: ThreadReference = marshaller.thread
  protected lazy val invoker = Invokers.shared.getDynamic(scriptObject)

  lazy val className: String = invoker.getClassName().asString

  lazy val isArray: Boolean = invoker.isArray().asBool(false)

  lazy val isRegularOrTypedArray: Boolean =
    isArray || className.endsWith("Array")

  def getProto(): Value = invoker.getProto()
  def setProto(proto: Value): Unit = invoker.setProto(proto)

  // For WithObject
  def getExpression(): ObjectReference = invoker.getExpression().asInstanceOf[ObjectReference]

  def put(key: AnyRef, value: AnyRef, isStrict: Boolean): Unit =
    invoker.applyDynamic(putObjectObjectBoolSignature)(key, value, isStrict)

  def get(key: AnyRef): ValueNode =
    invoker.applyDynamic(getObjectSignature)(key)

  def getString(key: AnyRef): String = invoker.applyDynamic(getObjectSignature)(key).asString
  def getInt(key: AnyRef, defaultValue: Int): Int = invoker.applyDynamic(getObjectSignature)(key).asNumber(defaultValue).intValue()

  def getRequiredInt(key: AnyRef): Int =
    invoker.applyDynamic(getObjectSignature)(key).asNumber(throw new IllegalStateException(s"Property $key doesn't have a number value")).intValue()

  def actualToString: String = invoker.applyDynamic("toString")().asString

  def typeOfObject(): String = {
    Try(invoker.applyDynamic(getObjectSignature)("constructor")) match {
      case Success(v) if marshaller.isScriptObject(v) =>
        // Marshalling via FunctionNode is expensive as it gets the source, so do this manually.
        val mirror = new ScriptObjectMirror(v.asInstanceOf[ObjectReference])
        mirror.maybeAsFunction.map(_.name).getOrElse(className)
      case _ =>
        // Fallback to the class name
        className
    }
  }

  override def toString = "ScriptObjectMirror (maybe you meant actualToString?)"

  def maybeAsFunction: Option[ScriptFunctionMirror] = {
    if (className == "Function") Some(new ScriptFunctionMirror(scriptObject))
    else None
  }

  def asFunction = maybeAsFunction.getOrElse(throw new IllegalStateException("asFunction can only be called for a function"))

  def asArray = {
    assert(isArray, "asArray can only be called for an array")
    new ScriptArrayMirror(scriptObject)
  }
}

class ScriptArrayMirror(scriptObject: ObjectReference)(implicit marshaller: Marshaller) extends ScriptObjectMirror(scriptObject) {
  import Mirrors._
  import ScriptObjectMirror._
  private implicit val thread: ThreadReference = marshaller.thread

  def length: Int = invoker.getLength().asNumber(0).intValue()

  def at(index: Int): Value = invoker.applyDynamic(getIntSignature)(index)
}

class ScriptFunctionMirror(scriptObject: ObjectReference)(implicit marshaller: Marshaller) extends ScriptObjectMirror(scriptObject) {
  import Mirrors._
  import com.programmaticallyspeaking.ncd.nashorn.JDIExtensions._
  private implicit val thread: ThreadReference = marshaller.thread

  lazy val name: String = invoker.getName().asString
  lazy val source: String = invoker.toSource().asString

  private lazy val isBound: Boolean = invoker.isBoundFunction().asBool(false)

  def boundTargetFunction: Option[ScriptFunctionMirror] = {
    if (isBound) {
      invoker.getTargetFunction() match {
        case objRef: ObjectReference => Some(new ScriptObjectMirror(objRef).asFunction)
        case other => throw new RuntimeException("Unexpected target function type: " + other)
      }
    } else None
  }

  /**
    * Invokes a no-argument function.
    * The result is not marshalled, because the caller may want to prevent GC for the result - which perhaps
    * is something Marshaller should do (in which case we can change to marshal here)?
    *
    * @return the native/JDI result
    */
  def invokeNoArgs(): Value = invoker.invoke(null, null)

  def invoke(thiz: Value, args: Seq[Value]): Value = invoker.invoke(thiz, args.toArray)

  //TODO: Correct to return Value here? Marshalling is non-trivial due to special target (ScopeObject), i.e.
  //TODO: Marshaller doesn't auto-marshal to ScopeObject since it cannot recognize a scope object ref.
  def scopes: Seq[Value] = {
    val scopeValue = invoker.getScope()
    Option(scopeValue).toSeq ++ scopeParents(scopeValue)
  }

  private def scopeParents(value: Value): Seq[Value] = value match {
    case v if v == null     => Seq.empty
    case v if v.isGlobal    => Seq.empty // because prototype of Global is Object, which isn't a scope
    case c: ObjectReference =>
      val proto = new ScriptObjectMirror(c).getProto()
      Option(proto).toSeq ++ scopeParents(proto)
    case _                  => Seq.empty
  }
}

object ScriptObjectMirror {
  val putObjectObjectBoolSignature = "put(Ljava/lang/Object;Ljava/lang/Object;Z)Ljava/lang/Object;"
  val getObjectSignature = "get(Ljava/lang/Object;)Ljava/lang/Object;"
  val getIntSignature = "get(I)Ljava/lang/Object;"
  val protoName = "__proto__"
}