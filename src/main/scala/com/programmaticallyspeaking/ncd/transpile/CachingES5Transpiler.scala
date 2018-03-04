package com.programmaticallyspeaking.ncd.transpile

class CachingES5Transpiler(inner: ES5Transpiler) extends ES5Transpiler {

  private object lock
  private var cache = Map.empty[String, String]

  override def transpile(input: String): String = lock.synchronized {
    cache.get(input) match {
      case Some(output) => output
      case None =>
        val output = inner.transpile(input)
        cache += input -> output
        output
    }

  }
}