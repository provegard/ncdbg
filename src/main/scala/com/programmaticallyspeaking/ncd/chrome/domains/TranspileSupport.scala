package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.transpile.{CachingES5Transpiler, ClosureBasedES5Transpiler}

trait TranspileSupport {

  // For transpiling ES6 code to ES5 that Nashorn pre-Java 10 (Java 9 in ES6 mode says 'not yet implemented'
  // for generator functions) understands.
  private val transpiler = new CachingES5Transpiler(new ClosureBasedES5Transpiler)

  private val transpileTrigger = "function\\s*\\*".r

  def needsTranspile(code: String): Boolean = transpileTrigger.findFirstIn(code).isDefined

  def transpile(code: String): String = transpiler.transpile(code)
}
