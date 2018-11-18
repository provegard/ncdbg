package com.programmaticallyspeaking.repl

import jdk.nashorn.api.scripting.NashornScriptEngineFactory

import scala.util.Try

class Engine {
  private val factory = new NashornScriptEngineFactory
  private val scriptEngine = factory.getScriptEngine("language=es6")
  private val bindings = scriptEngine.createBindings()

  def evaluate(s: String): Try[AnyRef] = {
    Try(scriptEngine.eval(s, bindings))
  }

}
