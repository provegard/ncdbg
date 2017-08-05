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
                   breakableLocationLookup: BreakableLocationLookup) extends Logging {
  import scala.collection.JavaConverters._

  private def scopeWithFreeVariables(scopeObject: Value, freeVariables: Map[String, AnyRef])(implicit marshaller: Marshaller): Value = {
    require(scopeObject != null, "Scope object must be non-null")
    implicit val thread = marshaller.thread
    // If there aren't any free variables, we don't need to create a wrapper scope
    if (freeVariables.isEmpty) return scopeObject

    // Just using "{}" returns undefined - don't know why - but "Object.create" works well.
    // Use the passed scope object as prototype object so that scope variables will be seen as well.
    var scopeObjFactory =
    s"""(function() { var obj = Object.create(this);
       |obj['${hiddenPrefix}changes']=[];
       |obj['${hiddenPrefix}resetChanges']=function(){ obj['${hiddenPrefix}changes'].length=0; };
       """.stripMargin

    // Add an accessor property for each free variable. This allows us to track changes to the variables, which is
    // necessary to be able to update local variables later on.
    freeVariables.foreach {
      case (name, _) =>
        scopeObjFactory +=
          s"""Object.defineProperty(obj,'$name',{
             |  get:function() { return this['$hiddenPrefix$name']; },
             |  set:function(v) { this['${hiddenPrefix}changes'].push('$name',v); this['$hiddenPrefix$name']=v; },
             |  enumerable:true
             |});
           """.stripMargin
    }
    scopeObjFactory += "return obj;}).call(this)"

    val anObject = codeEval.eval(scopeObject, null, scopeObjFactory).asInstanceOf[ObjectReference]
    val mirror = new ScriptObjectMirror(anObject)
    freeVariables.foreach {
      case (name, value) =>
        val valueToPut = value match {
          case prim: PrimitiveValue => boxer.boxed(prim)
          case other => other
        }
        mirror.put(hiddenPrefix + name, valueToPut, isStrict = false)
    }

    anObject
  }

  private def scopeTypeFromValueType(value: Value): ScopeType = {
    val typeName = value.`type`().name()
    // jdk.nashorn.internal.objects.Global
    if (typeName.endsWith(".Global"))
      return ScopeType.Global
    // jdk.nashorn.internal.runtime.WithObject
    if (typeName.endsWith(".WithObject"))
      return ScopeType.With
    ScopeType.Closure
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
    def toScope(v: Value) = Scope(marshaller.marshal(v), scopeTypeFromValueType(v))
    def findGlobalScope(): Option[Scope] = {
      if (Option(thisValue).map(scopeTypeFromValueType).contains(ScopeType.Global)) {
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
            val originalScope = values.get(":scope")

            // Variables that don't start with ":" are locals
            val localValues = values.filter(e => !e._1.startsWith(":")) // for use in evaluateCodeOnFrame

            val thisObj = marshaller.marshal(originalThis)

            // The scope may be missing, in which case we use 'this'. Which may be null (different from missing), in
            // which case we use global as scope.
            val scopeObject = originalScope
              .orElse(Option(originalThis))
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
              case (code, namedValues) =>
                // Create a scope object for the extra variables to use during evaluation, if any.
                val scopeToUse = scopeWithFreeVariables(localScope, namedValues)

                try {
                  val ret = codeEval.eval(originalThis, scopeToUse, code)
                  marshaller.marshal(ret) match {
                    case SimpleValue(str: String) if str == EvaluatedCodeMarker =>
                      // A non-expression statements such as "var x = 42" causes the evaluation marker to leak as an
                      // expression result. We suppress it here!
                      SimpleValue(Undefined)
                    case other => other
                  }
                } catch {
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
