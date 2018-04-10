package com.programmaticallyspeaking.ncd.nashorn

import java.util.Collections

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.Undefined
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.programmaticallyspeaking.ncd.javascript.Minifier
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost._
import com.programmaticallyspeaking.ncd.nashorn.StackBuilder.BreakableLocationLookup
import com.programmaticallyspeaking.ncd.nashorn.TypeConstants.NIR_Context
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi._
import org.slf4s.Logging

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

object StackBuilder {
  trait BreakableLocationLookup {
    def apply(location: Location): Option[BreakableLocation]
  }

  // Note: __noSuchProperty__ is a workaround for the fact that we're not creating a true scope object.
  // Nashorn has a class Scope which is a ScriptObject subclass that overrides isScope() and returns true, but
  // creating a Scope instance is quite involved as far as I can tell.
  private val scopeFactoryExpr =
    s"""
       |(function () {
       |  var varNames = [], storage = {}, scope = Object.create(this), name, value;
       |  for (var i = 0, j = arguments.length; i < j; i+=2) {
       |    name = arguments[i];
       |    value = arguments[i+1];
       |    storage[name] = value;
       |    (function (name) {
       |      Object.defineProperty(scope, name, {
       |        get: function() { return storage[name]; },
       |        set: function(v) { this['${hiddenPrefix}changes'].push(name,v); storage[name]=v; },
       |        enumerable: true
       |      });
       |    })(name);
       |  }
       |  scope['${hiddenPrefix}changes'] = [];
       |  scope['${hiddenPrefix}resetChanges'] = function() { scope['${hiddenPrefix}changes'].length=0; };
       |  scope.__noSuchProperty__ = function(prop) { throw new ReferenceError('"' + prop + '" is not defined'); };
       |  return scope;
       |})
     """.stripMargin
  val scopeFactoryExprMinified = Minifier.minify(scopeFactoryExpr)
}

class StackBuilder(stackframeIdGenerator: IdGenerator, typeLookup: TypeLookup, mappingRegistry: MappingRegistry, codeEval: CodeEval, boxer: Boxer,
                   breakableLocationLookup: BreakableLocationLookup, gCContext: GCContext) extends Logging {
  import scala.collection.JavaConverters._
  import JDIExtensions._

  private def scopeWithFreeVariables(scopeObject: Value, freeVariables: Map[String, AnyRef])(implicit marshaller: Marshaller): Value = {
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
    def stringValue(s: String): Value = thread.virtualMachine().mirrorOf(s)

    val aFunction = codeEval.eval(None, None, StackBuilder.scopeFactoryExprMinified, Lifecycle.Paused).asInstanceOf[ObjectReference]
    val mirror = new ScriptObjectMirror(aFunction).asFunction

    val args = varNames.map(stringValue).zip(varValues).flatMap(p => Seq(p._1, p._2))
    val anObject = gCContext.pin(Lifecycle.Paused)(mirror.invoke(scopeObject, args))

    anObject
  }

  private def buildStackFramesSequence(perStackFrame: Seq[(Map[String, Value], Location)])(implicit marshaller: Marshaller): Seq[StackFrameHolder] = {
    implicit val thread: ThreadReference = marshaller.thread
    val scopeManager = new ScopeManager(typeLookup)

    var originalScopeOfLastScriptFrame: Option[Value] = None

    // Traverse the scopes bottom-up (reversed), since then ScopeManager should be able to reuse a lot
    // of knowledge it gains on the way. In the end, we'll reverse again to get a proper result order.
    val reversed = perStackFrame.zipWithIndex.reverse.map {
      case ((values, location), stackIdx) =>
        val functionMethod = location.method()

        // Generate an ID for the stack frame so that we can find it later when asked to evaluate code for a
        // particular stack frame.
        val stackframeId = stackframeIdGenerator.next

        // ":this" should always be present, but a function that doesn't capture anything may lack a ":scope" variable
        values.get(":this") match {
          case Some(originalThis) =>
            val originalScope = values.get(":scope")

            // If the original scope is such that it's the same one as that of the last script frame, then
            // this frame doesn't have its own scope.
            val stackFrameHasNoOwnScope = originalScope.exists(originalScopeOfLastScriptFrame.contains)
            originalScopeOfLastScriptFrame = originalScope

            // Variables that don't start with ":" are locals
            val localValues = values.filter(e => !e._1.startsWith(":")) // for use in evaluateCodeOnFrame

            val thisObj = marshaller.marshal(originalThis)

            // The scope may be missing/null/undefined, which is fine.
            val scopeObject = originalScope.collect { case s if !s.isUndefined => s }.orNull

            // Figure out the scope chain. 'scopeObject' (of non-null) will be last.
            var scopes = scopeManager.scopeChainTopLast(scopeObject)

            // Do we need an artificial scope? Always when we have local variables, because they are not
            // necessarily captured by a Nashorn-generated scope.
            //TODO: When the code contains eval, there should be a complete Nashorn-generated scope. Can we detect?
            val needsArtificialLocalScope = localValues.nonEmpty

            val optArtificialLocalScope = if (needsArtificialLocalScope) {

              val (parentOfArtificial, childOfArtificial) = if (stackFrameHasNoOwnScope) {
                (Option(scopeObject), None)
              } else {
                val (withScopesReversed, baseScopesReversed) = scopes.reverse.span(_.scopeType == ScopeType.With)
                (baseScopesReversed.headOption, withScopesReversed.lastOption)
              }

              val artificialScope = scopeWithFreeVariables(parentOfArtificial.orNull, localValues)

              // Let the scope that logically follows the artificial scope have it as prototype.
              childOfArtificial.foreach { c =>
                val mirror = new ScriptObjectMirror(c.asInstanceOf[ObjectReference])
                mirror.setProto(artificialScope)
              }

              // Create an artificial object node to hold the locals. Note that the object ID must be unique per stack
              // since we store object nodes in a map.
              val objectId = ObjectId(localScopeObjectIdPrefix + stackframeId)
              val node = ObjectNode("Object", objectId)

              // Note: Don't register locals as extra properties, since they will shadow the real properties on the
              // local scope object.
              mappingRegistry.register(artificialScope, node, Map(stackFrameIndexExtraProp -> SimpleValue(stackIdx)))

              // Rewrite the scopes list by inserting the artificial scopes in the appropriate position
              val (before, after) = scopes.span(s => !childOfArtificial.contains(s))
              scopes = (before :+ artificialScope) ++ after

              // Let the scope manager know about our now scope, so it can update its caches.
              scopeManager.registerArtificialLocalScope(artificialScope, node, parentOfArtificial, childOfArtificial)

              Some(artificialScope)
            } else None

            val optTopScope = scopes.lastOption
            val scopeChain = scopeManager.createScopeChain((originalThis, thisObj), optArtificialLocalScope, scopes)

            def evaluateCodeOnFrame: CodeEvaluator = {
              case (code, maybeInvokeFunctionData) =>
                try {
                  // If we expect the result of eval to be a function, make sure to wrap the function declaration
                  // in parentheses so that the function is returned.
                  val actualCode = if (maybeInvokeFunctionData.isDefined) s"($code)" else code
                  var ret = codeEval.eval(Some(originalThis), optTopScope, actualCode, Lifecycle.Paused)

                  maybeInvokeFunctionData match {
                    case Some(data) if ret.isInstanceOf[ObjectReference] =>
                      // interpret 'ret' as a function and call it with the arguments
                      val mirror = new ScriptObjectMirror(ret.asInstanceOf[ObjectReference]).asFunction
                      ret = gCContext.pin(Lifecycle.Paused)(mirror.invoke(data.thisValue, data.arguments))
                    case _ =>
                      // marshal 'ret' unchanged
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
    reversed.reverse
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

