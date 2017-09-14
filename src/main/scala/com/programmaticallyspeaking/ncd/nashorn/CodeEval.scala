package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost._
import com.sun.jdi._

class CodeEval(typeLookup: TypeLookup, preventGC: (Value, Lifecycle.EnumVal) => Unit) {
  import TypeConstants._

  def eval(thisObject: Value, scopeObject: Value, code: String, lifecycle: Lifecycle.EnumVal)(implicit thread: ThreadReference): Value = {
    // We've observed ObjectCollectedException while disabling GC... Try a few times before giving up!
    eval(thisObject, scopeObject, code, lifecycle, 3)
  }

  private def eval(thisObject: Value, scopeObject: Value, code: String, lifecycle: Lifecycle.EnumVal, attemptsLeft: Int)(implicit thread: ThreadReference): Value = {
    val v = DebuggerSupport_eval_custom(thisObject, scopeObject, code)
    try {
      preventGC(v, lifecycle)
      v
    } catch {
      case e: ObjectCollectedException if attemptsLeft > 0 =>
        eval(thisObject, scopeObject, code, lifecycle, attemptsLeft - 1)
    }
  }

  /** Custom version of jdk.nashorn.internal.runtime.DebuggerSupport.eval that makes a difference between a returned
    * exception value and a thrown exception value. If we use the real DebuggerSupport.eval, there's no way to know
    * if an actual Java exception was returned or thrown as a result of an evaluation error.
    *
    * @param thread the thread to use when invoking methods
    * @param thisObject the object to use as `this`. If `null`, the global object will be used.
    * @param scopeObject the object to use as scope. If `null`, the global object will be used.
    * @param code the code to evaluate
    * @return the result of the evaluation. A thrown exception is wrapped in a [[ThrownExceptionReference]] instance.
    */
  private def DebuggerSupport_eval_custom(thisObject: Value, scopeObject: Value, code: String)(implicit thread: ThreadReference): Value = {
    // Based on the following code:
    //    static Object eval(ScriptObject scope, Object self, String string, boolean returnException) {
    //      Global global = Context.getGlobal();
    //      Object initialScope = scope != null?scope:global;
    //      Object callThis = self != null?self:global;
    //      Context context = global.getContext();
    //
    //      try {
    //        return context.eval((ScriptObject)initialScope, string, callThis, ScriptRuntime.UNDEFINED);
    //      } catch (Throwable var9) {
    //        return returnException?var9:null;
    //      }
    //    }
    typeLookup(NIR_Context) match {
      case Some(ct: ClassType) =>
        val invoker = Invokers.shared.getStatic(ct)

        val global = invoker.getGlobal()
        val globalInvoker = Invokers.shared.getDynamic(global.asInstanceOf[ObjectReference])
        val initialScope = if (scopeObject != null) scopeObject else global
        val callThis = if (thisObject != null) thisObject else global
        val context = globalInvoker.getContext()
        val contextInvoker = Invokers.shared.getDynamic(context.asInstanceOf[ObjectReference])

        try {
          val codeWithMarker = s"""'$EvaluatedCodeMarker';$code"""
          contextInvoker.eval(initialScope, codeWithMarker, callThis, null)
        } catch {
          case ex: InvocationFailedException =>
            new ThrownExceptionReference(thread.virtualMachine(), ex.exceptionReference)
        }

      case _ =>
        throw new IllegalStateException("The Context type wasn't found, cannot evaluate code.")
    }
  }
}
