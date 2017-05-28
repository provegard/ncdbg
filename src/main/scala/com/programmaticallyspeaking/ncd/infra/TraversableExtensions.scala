package com.programmaticallyspeaking.ncd.infra

import scala.collection.TraversableLike
import scala.language.implicitConversions

object TraversableExtensions {

  implicit def traversable2Extended[A, Repr](t: TraversableLike[A, Repr]): ExtTraversable[A, Repr] = new ExtTraversable[A, Repr](t)

  class ExtTraversable[A, Repr](t: TraversableLike[A, Repr]) {

    // https://stackoverflow.com/questions/3912753/scala-remove-duplicates-in-list-of-objects#answer-3912995
    def distinctBy[R](keyFun: (A) => R): Repr = {
      t.filterNot{ var set = Set[R]()
        obj => val k = keyFun(obj); val b = set(k); set += k; b}
    }
  }
}
