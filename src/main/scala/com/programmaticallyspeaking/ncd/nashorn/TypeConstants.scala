package com.programmaticallyspeaking.ncd.nashorn

object TypeConstants {

  val NIR_ScriptRuntime = "jdk.nashorn.internal.runtime.ScriptRuntime"
  val NIR_ECMAException = "jdk.nashorn.internal.runtime.ECMAException"
  val NIR_JSType = "jdk.nashorn.internal.runtime.JSType"
  val NIR_Context = "jdk.nashorn.internal.runtime.Context"
  val NIR_Source = "jdk.nashorn.internal.runtime.Source"
  val NIO_Global = "jdk.nashorn.internal.objects.Global"
  val JL_Boolean = "java.lang.Boolean"
  val JL_Integer = "java.lang.Integer"
  val JL_Long = "java.lang.Long"
  val JL_Double = "java.lang.Double"

  // The name of the DEBUGGER method in the ScriptRuntime class
  val ScriptRuntime_DEBUGGER = "DEBUGGER"

  // The Global.print method
  val Global_print = "print"

  // The Global.println method
  val Global_println = "println"
}
