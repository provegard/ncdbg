package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ValueNode
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost._
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi._

trait CompiledScriptRunner {
  def run()(implicit marshaller: Marshaller): ValueNode
}

object CodeEval {
  val EvalSourceName = "<ncdbg_eval>"
}

class CodeEval(typeLookup: TypeLookup, gCContext: GCContext) {
  import TypeConstants._

  /**
    * Evaluates code and returns the result. The evaluation is attempted at most 3 times
    * (since it may fail due to GC activity in the target VM).
    *
    * @param thisObject the object that is `this` during code evaluation
    * @param scopeObject scope object, for lookup of free variables
    * @param code the code to evaluate
    * @param lifecycle lifecycle for GC prevention of the result
    * @param thread thread for code evaluation
    * @return the evaluation result
    */
  def eval(thisObject: Option[Value], scopeObject: Option[Value], code: String, lifecycle: Lifecycle.EnumVal)(implicit thread: ThreadReference): Value = {
    // We've observed ObjectCollectedException while disabling GC... Try a few times before giving up!
    gCContext.pin(lifecycle) {
      DebuggerSupport_eval_custom(thisObject.orNull, scopeObject.orNull, code)
    }
  }

  //TODO: I don't think we should marshal here. The caller should do that. But ScriptObjectMirror takes an implicit Marshaller, so...
  private def runCompiledScript(scriptFunction: ObjectReference, lifecycle: Lifecycle.EnumVal)(implicit marshaller: Marshaller): ValueNode = {
    // NashornScriptEngine.evalImpl:
    //  var7 = ScriptObjectMirror.translateUndefined(ScriptObjectMirror.wrap(ScriptRuntime.apply(script, ctxtGlobal, new Object[0]), ctxtGlobal));
    val mirror = new ScriptObjectMirror(scriptFunction).asFunction
    try {
      val value = gCContext.pin(lifecycle)(mirror.invokeNoArgs())
      marshaller.marshal(value)
    } catch {
      case ex: InvocationFailedException =>
        val v = new ThrownExceptionReference(marshaller.thread.virtualMachine(), ex.exceptionReference)
        marshaller.marshal(v)
    }
  }

  def compileScript(script: String, url: String, scopeObject: AnyRef, lifecycle: Lifecycle.EnumVal)(implicit marshaller: Marshaller): CompiledScriptRunner = {
    val fun = gCContext.pin(lifecycle) {
      implicit val thread = marshaller.thread
      NashornScriptEngine_asCompiledScript(script, url, scopeObject)
    }
    new CompiledScriptRunner {
      override def run()(implicit marshaller: Marshaller): ValueNode =
        runCompiledScript(fun, Lifecycle.Paused)
    }
  }

  private def NashornScriptEngine_asCompiledScript(script: String, url: String, scopeObject: AnyRef)(implicit threadReference: ThreadReference): ObjectReference = {
    // NashornScriptEngine.asCompiledScript:
    //  1. src       = Source.sourceFor("<compiled>", script, true)
    //  2. ctx       = Context.getContext()
    //  3. mgcs      = ctx.compileScript(src:Source)
    //  4. scriptFun = mgcs.getFunction(Context.getGlobal())
    (for {
      contextType <- typeLookup(NIR_Context)
      sourceType <- typeLookup(NIR_Source)
    } yield (contextType, sourceType)) match {
      case Some((contextType: ClassType, sourceType: ClassType)) =>
        // 1
        val sourceInvoker = Invokers.shared.getStatic(sourceType)
        val sig = "sourceFor(Ljava/lang/String;Ljava/lang/String;)Ljdk/nashorn/internal/runtime/Source;"
        val source = sourceInvoker.applyDynamic(sig)(url, script)

        // 2
        val contextClassInvoker = Invokers.shared.getStatic(contextType)
        val context = contextClassInvoker.getContext().asInstanceOf[ObjectReference]

        // 3
        val contextDynInvoker = Invokers.shared.getDynamic(context)
        if (scopeObject != null) {
          // Returns ScriptFunction directly
          contextDynInvoker.compileScript(source, scopeObject).asInstanceOf[ObjectReference]
        } else {
          val multiGlobalCompiledScript = contextDynInvoker.compileScript(source).asInstanceOf[ObjectReference]

          // 4
          val global = contextClassInvoker.getGlobal()
          val mgcsInvoker = Invokers.shared.getDynamic(multiGlobalCompiledScript)
          mgcsInvoker.getFunction(global).asInstanceOf[ObjectReference]
        }
      case _ =>
        throw new IllegalStateException("The Context and/or Source type wasn't found, cannot compile a script.")
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
          // location + eval = direct eval
          // which is necessary to make evaluation of same code work across artificial and real scopes, it seems
          contextInvoker.eval(initialScope, code, callThis, CodeEval.EvalSourceName, /*strict:*/ false, /*eval:*/ true)
        } catch {
          case ex: InvocationFailedException =>
            new ThrownExceptionReference(thread.virtualMachine(), ex.exceptionReference)
        }

      case _ =>
        throw new IllegalStateException("The Context type wasn't found, cannot evaluate code.")
    }
  }
}
