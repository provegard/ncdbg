package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi._

class Boxer(typeLookup: TypeLookup) {
  import TypeConstants._

  private def boxed(prim: PrimitiveValue, typeName: String, method: String)(implicit thread: ThreadReference): Value = {
    typeLookup(typeName) match {
      case Some(aType) =>
        val invoker = Invokers.shared.getStatic(aType)
        invoker.applyDynamic(method)(prim)
      case None => throw new IllegalArgumentException(s"Failed to find the '$typeName' type.")
    }
  }

  /**
    * Performs boxing of a primitive value, e.g. int => Integer
    * @param thread the thread to run boxing methods on
    * @param prim the primitive value to box
    * @return the boxed value
    */
  def boxed(prim: PrimitiveValue)(implicit thread: ThreadReference): Value = prim match {
    case b: BooleanValue => boxed(b, JL_Boolean, "valueOf(Z)Ljava/lang/Boolean;")
    case i: IntegerValue => boxed(i, JL_Integer, "valueOf(I)Ljava/lang/Integer;")
    case l: LongValue =>
      // LongValue is kept for completeness - Nashorn since8 8u91 or something like that doesn't use Long for
      // representing numbers anymore.
      boxed(l, JL_Long, "valueOf(J)Ljava/lang/Long;")
    case d: DoubleValue => boxed(d, JL_Double, "valueOf(D)Ljava/lang/Double;")
    case _ => throw new IllegalArgumentException("Cannot box " + prim)
  }

  def unboxed(objectValue: ObjectReference): Value = {
    // This is ugly, but if we invoke a method like 'Integer.intValue', we will invalidate any stack frame where we
    // need to set the resulting value to a variable...
    val valueField = objectValue.referenceType().fieldByName("value")
    Option(valueField).map(objectValue.getValue).getOrElse(objectValue)
  }
}
