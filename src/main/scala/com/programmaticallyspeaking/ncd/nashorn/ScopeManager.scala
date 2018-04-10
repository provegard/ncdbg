package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Scope, ScopeType, ValueNode}
import com.programmaticallyspeaking.ncd.nashorn.TypeConstants.NIR_Context
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi.{ObjectReference, ThreadReference, Value}

import scala.collection.mutable

class ScopeManager(typeLookup: TypeLookup)(implicit marshaller: Marshaller) {
  import JDIExtensions._
  private val cachedParentOf = mutable.Map[Value, Value]()
  private val marshalledScope = mutable.Map[Value, ValueNode]()

  private def getProtoOf(v: Value): Value = v match {
    case x if x.scopeType == ScopeType.Global => null
    case x: ObjectReference =>
      val m = new ScriptObjectMirror(x)
      m.getProto()
    case _ => null
  }

  def parentOf(scope: Value): Value = {
    cachedParentOf.getOrElseUpdate(scope, getProtoOf(scope))
  }

  def marshalScope(scope: Value): ValueNode = {
    assert(scope != null, "Won't marshall a null scope")
    marshalledScope.getOrElseUpdate(scope, marshaller.marshal(scope))
  }

  def scopeChainTopLast(scope: Value): List[Value] = {
    if (scope == null) List.empty
    else if (scope.scopeType == ScopeType.Global) scope :: Nil
    else scopeChainTopLast(parentOf(scope)) :+ scope
  }

  def registerArtificialLocalScope(scope: Value, marshalled: ValueNode, parent: Option[Value], child: Option[Value]): Unit = {
    marshalledScope += scope -> marshalled
    cachedParentOf += scope -> parent.orNull
    child.foreach { c =>
      cachedParentOf += c -> scope
    }
  }

  def createScopeChain(thisTuple: (Value, ValueNode), localScope: Option[Value], scopesBottomUp: Seq[Value])(implicit marshaller: Marshaller): Seq[Scope] = {
    implicit val thread: ThreadReference = marshaller.thread

    def scopeType(v: Value): ScopeType = {
      if (localScope.contains(v)) ScopeType.Local
      else v.scopeType
    }

    def toScope(v: Value): Scope = {
      Scope(marshalScope(v), scopeType(v))
    }

    def findGlobalScope(): Option[Scope] = {
      if (Option(thisTuple._1).map(_.scopeType).contains(ScopeType.Global)) {
        // this == global, so no need to call Context.getGlobal().
        Some(Scope(thisTuple._2, ScopeType.Global))
      } else {
        getGlobal().map(toScope)
      }
    }

    var scopes = scopesBottomUp.reverse.map(toScope)

    // Make sure we have a global scope lsat!
    if (!scopes.exists(_.scopeType == ScopeType.Global)) {
      scopes ++= findGlobalScope()
    }
    scopes
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
}
