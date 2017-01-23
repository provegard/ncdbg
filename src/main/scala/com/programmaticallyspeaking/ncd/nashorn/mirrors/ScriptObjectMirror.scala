package com.programmaticallyspeaking.ncd.nashorn.mirrors

import com.programmaticallyspeaking.ncd.nashorn.{DynamicInvoker, MissingMethodException}
import com.sun.jdi._

/**
  * Mirror for `jdk.nashorn.internal.runtime.ScriptObject`. This class doesn't do marshalling - the return value of
  * methods is typically a JDI [[com.sun.jdi.Value]].
  *
  * @param thread thread on which method calls will take place
  * @param scriptObject the `ScriptObject` instance to interact with
  */
class ScriptObjectMirror(thread: ThreadReference, val scriptObject: ObjectReference) {
  import ScriptObjectMirror._

  lazy val invoker = new DynamicInvoker(thread, scriptObject)

  def getClassName = invoker.getClassName()

  def isArray = invoker.isArray()

  def entrySet() = invoker.entrySet().asInstanceOf[ObjectReference]
  def propertyIterator() = invoker.propertyIterator()

  def set(key: AnyRef, value: AnyRef, isStrict: Boolean) = {
    val scriptObjectFlags = if (isStrict) 2 else 0; // taken from ScriptObject.put
    // Figure out set overload
    // The LongValue case is untested since Nashorn since 8u91 or something has removed the use of Long. However the
    // case is kept for completeness.
    val valueSigType = value match {
      case i: IntegerValue => "I"
      case d: DoubleValue => "D"
      case l: LongValue => "J" // note: untested
      case _ => "Ljava/lang/Object;"
    }
    // Different Java versions have different parameter lists
    val withFlags = s"set(Ljava/lang/Object;${valueSigType}I)V"
    try {
      invoker.applyDynamic(withFlags)(key, value, scriptObjectFlags)
    } catch {
      case ex: MissingMethodException =>
        // Retry with a method that doesn't take a flags (int) argument
        val withoutFlags = s"set(Ljava/lang/Object;$valueSigType)V"
        invoker.applyDynamic(withoutFlags)(key, value)
    }
  }

  def get(key: AnyRef) =
    invoker.applyDynamic(getObjectSignature)(key)

  def actualToString = invoker.applyDynamic("toString")()

  override def toString = "ScriptObjectMirror (maybe you meant actualToString?)"
}

object ScriptObjectMirror {
  val getObjectSignature = "get(Ljava/lang/Object;)Ljava/lang/Object;"
}