package com.programmaticallyspeaking.ncd.ioc

import java.lang.reflect.Constructor

/**
  * An extremely simple IoC container.
  *
  * @param references the references used for creating instances
  */
class Container(references: Seq[AnyRef]) {
  private def satisfiesType(t: Class[_], ref: AnyRef): Boolean = {
    if (ref == null) return false
    t.isInstance(ref)
  }

  private def findReference(t: Class[_]): AnyRef = {
    val matcher = satisfiesType(t, _: AnyRef)
    references.find(matcher) match {
      case Some(ref) => ref
      case None => throw new IllegalArgumentException(s"No reference matching parameter type $t")
    }
  }

  /**
    * Creates a new instance of the given class by satisfying the parameters of its only public constructor. It is an
    * error if the class has more than one public constructor or no public constructors.
    *
    * @param clazz the class to create an instance of
    * @tparam R the type of the instance
    * @return a newly created instance
    */
  def newInstance[R <: AnyRef](clazz: Class[R]): R = {
    val ca = constructionFor(clazz)
    ca._1.newInstance(ca._2: _*).asInstanceOf[R]
  }

  /**
    * Creates an instance of the given class using a special creator function. The signature of the creator function
    * is a bit odd, but it's meant to be used with `akka.actor.Props`.
    *
    * @param clazz the class to create an instance of
    * @param creator creator function
    * @tparam R the type of the instance
    * @return a newly created instance
    */
  def newInstance[R](clazz: Class[_], creator: (Class[_], Seq[Any]) => R): R = {
    val ca = constructionFor(clazz)
    creator(clazz, ca._2)
  }

  private def constructionFor(clazz: Class[_]): (Constructor[_], Seq[AnyRef]) = {
    clazz.getConstructors.toList match {
      case c :: Nil =>
        val args = c.getParameterTypes.map(findReference)
        (c, args)

      case _ :: _ => throw new IllegalArgumentException(s"Class $clazz has multiple public constructors")
      case Nil => throw new IllegalArgumentException(s"Class $clazz has no public constructor")
    }
  }
}

object Container {
  val empty: Container = new Container(Seq.empty)
}