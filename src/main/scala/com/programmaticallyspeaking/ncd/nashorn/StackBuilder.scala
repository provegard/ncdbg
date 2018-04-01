package com.programmaticallyspeaking.ncd.nashorn

import java.util.Collections

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost._
import com.programmaticallyspeaking.ncd.nashorn.StackBuilder.BreakableLocationLookup
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi._
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer
import scala.util.Try

object StackBuilder {
  trait BreakableLocationLookup {
    def apply(location: Location): Option[BreakableLocation]
  }
}

class StackBuilder(stackframeIdGenerator: IdGenerator, typeLookup: TypeLookup, mappingRegistry: MappingRegistry, codeEval: CodeEval, boxer: Boxer,
                   breakableLocationLookup: BreakableLocationLookup, gCContext: GCContext) extends Logging {
  import scala.collection.JavaConverters._
  import JDIExtensions._
  import TypeConstants._

  private def scopeWithFreeVariables(scopeObject: Value, freeVariables: Map[String, AnyRef])(implicit marshaller: Marshaller): Value = {
    require(scopeObject != null, "Scope object must be non-null")
    implicit val thread = marshaller.thread
    // If there aren't any free variables, we don't need to create a wrapper scope
    if (freeVariables.isEmpty) return scopeObject

    val entries: Seq[(String, AnyRef)] = freeVariables.toSeq
    val varNames = entries.map(_._1)
    val varValues: Seq[Value] = entries.map(_._2 match {
      case prim: PrimitiveValue => boxer.boxed(prim)
      case v: Value => v
      case null => null
      case other => throw new UnsupportedOperationException("Don't know how to handle: " + other)
    })

    val jsVarNames = varNames.mkString("['", "','", "']")

    // Note: __noSuchProperty__ is a workaround for the fact that we're not creating a true scope object.
    // Nashorn has a class Scope which is a ScriptObject subclass that overrides isScope() and returns true, but
    // creating a Scope instance is quite involved as far as I can tell.
    val funcExpression =
      s"""
         |(function () {
         |  var varNames=$jsVarNames,storage={},scope=Object.create(this);
         |  for (var i=0,j=varNames.length;i<j;i++) {
         |    storage[varNames[i]] = arguments[i];
         |    (function(name){
         |      Object.defineProperty(scope,name,{
         |        get:function(){return storage[name];},
         |        set:function(v){this['${hiddenPrefix}changes'].push(name,v);storage[name]=v;},
         |        enumerable:true
         |      });
         |    })(varNames[i]);
         |  }
         |  scope['${hiddenPrefix}changes']=[];
         |  scope['${hiddenPrefix}resetChanges']=function(){ scope['${hiddenPrefix}changes'].length=0; };
         |  scope.__noSuchProperty__=function(prop){throw new ReferenceError('"' + prop + '" is not defined');};
         |  return scope;
         |})
       """.stripMargin

    val aFunction = codeEval.eval(None, None, funcExpression, Lifecycle.Paused).asInstanceOf[ObjectReference]
    val mirror = new ScriptObjectMirror(aFunction).asFunction

    val anObject = gCContext.pin(Lifecycle.Paused)(mirror.invoke(scopeObject, varValues))

    anObject
  }

  private def prototypeOf(value: Value)(implicit thread: ThreadReference): Option[Value] = value match {
    case ref: ObjectReference =>
      val invoker = Invokers.shared.getDynamic(ref)
      val maybeProto = Option(invoker.getProto())
      // Prototype of Global is jdk.nashorn.internal.objects.NativeObject$Prototype - ignore it
      if (maybeProto.exists(_.`type`().name().endsWith("$Prototype"))) None
      else maybeProto
    case _ => None
  }

  private def prototypeChain(value: Value)(implicit thread: ThreadReference): Seq[Value] = prototypeOf(value) match {
    case Some(proto) => Seq(proto) ++ prototypeChain(proto)
    case None => Seq.empty
  }

  private def getGlobal()(implicit thread: ThreadReference): Option[Value] = typeLookup(NIR_Context) match {
    case Some(context) =>
      val invoker = Invokers.shared.getStatic(context)
      val global = invoker.getGlobal()
      Option(global)
    case None =>
      // No global found :-(
      None
  }

  private def createScopeChain(marshaller: Marshaller, originalScopeValue: Option[Value], thisValue: Value, marshalledThisNode: ValueNode, localNode: Option[ObjectNode]): Seq[Scope] = {
    implicit val thread: ThreadReference = marshaller.thread
    // Note: I tried to mimic how Chrome reports scopes, but it's a bit difficult. For example, if the current scope
    // is a 'with' scope, there is no way (that I know of) to determine if we're in a function (IIFE) inside a with
    // block or if we're inside a with block inside a function.
    def toScope(v: Value) = Scope(marshaller.marshal(v), v.scopeType)
    def findGlobalScope(): Option[Scope] = {
      if (Option(thisValue).map(_.scopeType).contains(ScopeType.Global)) {
        // this == global, so no need to call Context.getGlobal().
        Some(Scope(marshalledThisNode, ScopeType.Global))
      } else {
        getGlobal().map(toScope)
      }
    }

    val scopeChain = ListBuffer[Scope]()

    // If we have locals, add a local scope
    localNode.foreach(scopeChain += Scope(_, ScopeType.Local))

    originalScopeValue.map(v => (v, toScope(v))) match {
      case Some((_, s)) if s.scopeType == ScopeType.Global =>
        // If the current scope is the global scope, add it but don't follow the prototype chain since it's unnecessary.
        scopeChain += s
      case Some((v, s)) =>
        // Add the scope and all its parent scopes
        scopeChain += s
        scopeChain ++= prototypeChain(v).map(toScope)
      case None =>
      // noop
    }

    // Make sure we have a global scope lsat!
    if (!scopeChain.exists(_.scopeType == ScopeType.Global)) {
      scopeChain ++= findGlobalScope()
    }

    scopeChain
  }

  private def buildStackFramesSequence(perStackFrame: Seq[(Map[String, Value], Location)])(implicit marshaller: Marshaller): Seq[StackFrameHolder] = {
    implicit val thread: ThreadReference = marshaller.thread
    perStackFrame.zipWithIndex.map {
      case ((values, location), stackIdx) =>
        val functionMethod = location.method()

        // Generate an ID for the stack frame so that we can find it later when asked to evaluate code for a
        // particular stack frame.
        val stackframeId = stackframeIdGenerator.next

        // ":this" should always be present, but a function that doesn't capture anything may lack a ":scope" variable
        values.get(":this") match {
          case Some(originalThis) =>
            var originalScope = values.get(":scope")
            //TODO: We didn't do this before... why?? If originalScope is null or undefined, should we leave it at that??
            originalScope = originalScope.flatMap { s =>
              if (s.`type`().name() == "jdk.nashorn.internal.runtime.Undefined") None
              else Some(s)
            }

            // Variables that don't start with ":" are locals
            val localValues = values.filter(e => !e._1.startsWith(":")) // for use in evaluateCodeOnFrame

            val thisObj = marshaller.marshal(originalThis)

            // The scope may be missing, in which case we use 'this'. Which may be null (different from missing), in
            // which case we use global as scope.
            val scopeObject = originalScope
              //.orElse(Option(originalThis))
              .orElse(getGlobal())
              .getOrElse(throw new IllegalStateException("Failed to locate a scope object. Tried scope, this, global."))

            // If needed, create a scope object to hold the local variables as "free" variables - so that evaluated
            // code can refer to them.
            // If we don't have :scope, use :this - it's used as a parent object for the created scope object.
            val localScope = scopeWithFreeVariables(scopeObject, localValues)

            // Create an artificial object node to hold the locals. Note that the object ID must be unique per stack
            // since we store object nodes in a map.
            val locals = localValues.map(e => e._1 -> marshaller.marshal(e._2))
            val localNode = if (locals.nonEmpty) {
              val objectId = ObjectId(localScopeObjectIdPrefix + stackframeId)
              val node = ObjectNode("Object", objectId)

              // Note: Don't register locals as extra properties, since they will shadow the real properties on the
              // local scope object.
              mappingRegistry.register(localScope, node, Map(stackFrameIndexExtraProp -> SimpleValue(stackIdx)))

              Some(node)
            } else None

            val scopeChain = createScopeChain(marshaller, originalScope, originalThis, thisObj, localNode)

            def evaluateCodeOnFrame: CodeEvaluator = {
              case (code, maybeInvokeFunctionData) =>
                try {
                  // If we expect the result of eval to be a function, make sure to wrap the function declaration
                  // in parentheses so that the function is returned.
                  val actualCode = if (maybeInvokeFunctionData.isDefined) s"($code)" else code
                  var ret = codeEval.eval(Some(originalThis), Some(localScope), actualCode, Lifecycle.Paused)

                  maybeInvokeFunctionData match {
//                    case Some(_) if !ret.isInstanceOf[ObjectReference] =>
//                      throw new RuntimeException(s"$ret is not an object reference that can be a function")
                    case Some(data) if ret.isInstanceOf[ObjectReference] =>
                      // interpret 'ret' as a function and call it with the arguments
                      val mirror = new ScriptObjectMirror(ret.asInstanceOf[ObjectReference]).asFunction
                      ret = withGCPrevention(Lifecycle.Paused)(mirror.invoke(data.thisValue, data.arguments))
                    case _ =>
                      // return 'ret' unchanged
                  }

                  marshaller.marshal(ret)
                } catch {
                  case ex: ObjectCollectedException =>
                    log.error("Object was garbage collected", ex)
                    throw new RuntimeException("Code evaluation error", ex)
                  case ex: Exception =>
                    // Don't log this at error level, because the error may be "ok". For example, if the user hovers over
                    // a property of a variable that contains undefined then DevTools will ask about the property value
                    // with silent errors, and when getting the value blows up we shouldn't be noisy!
                    log.debug("Code evaluation failed.", ex)
                    throw ex
                }
            }

            try {
              breakableLocationLookup.apply(location).map(bl => StackFrameImpl(stackframeId, thisObj, scopeChain, bl, evaluateCodeOnFrame, functionDetails(functionMethod))) match {
                case Some(sf) => StackFrameHolder(Some(sf), location)
                case None =>
                  log.warn(s"Won't create a stack frame for location ($location) since we don't recognize it.")
                  StackFrameHolder(None, location)
              }
            } catch {
              case ex: AbsentInformationException =>
                log.warn(s"Won't create a stack frame for location ($location) since there's no source information.")
                StackFrameHolder(None, location)
            }
          case _ =>
            StackFrameHolder(None, location)
        }

    }
  }

  def captureStackFrames(thread: ThreadReference)(implicit marshaller: Marshaller): Seq[StackFrameHolder] = {
    // Get all Values FIRST, before marshalling. This is because marshalling requires us to call methods, which
    // will temporarily resume threads, which causes the stack frames to become invalid.
    val perStackFrame = thread.frames().asScala.map { sf =>
      // In the step tests, I get JDWP error 35 (INVALID SLOT) for ':return' and since we don't use it, leave it for
      // now. If we need it, we can get it separately.
      val variables = Try(sf.visibleVariables()).getOrElse(Collections.emptyList()).asScala.filter(_.name() != ":return").asJava
      try {
        val values = sf.getValues(variables).asScala.map(e => e._1.name() -> e._2)
        (values.toMap, sf.location())
      } catch {
        case JDWP_ERR_INVALID_SLOT(_) =>
          val scalaVars = variables.asScala
          val vars = scalaVars.map(_.name()).mkString(", ")
          log.warn(s"INVALID_SLOT error for: $vars")

          // Get variable values one by one, and ignore the ones that result in INVALID_SLOT.
          // Another idea is to generate a fake value, something like "@@ERR:INVALID_SLOT". Good?
          val entries = scalaVars.flatMap(v => {
            try Some(v.name() -> sf.getValue(v)) catch {
              case JDWP_ERR_INVALID_SLOT(_) =>
                log.warn(s"INVALID_SLOT error for variable '${v.name()}', ignoring")
                None
            }
          })
          (entries.toMap, sf.location())
      }
    }

    // Second pass, marshal
    buildStackFramesSequence(perStackFrame)
  }

  private def functionDetails(functionMethod: Method): FunctionDetails = {
    FunctionDetails(functionMethod.name())
  }
}
