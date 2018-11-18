package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.infra.CompiledScript

object NameConvert {
  def sourceNameToUrl(typeName: => String, sourceName: String): String = sourceName match {
    case x if x.contains("<eval>") =>
      // For evaluated scripts, convert the type name into something that resembles a file URI.
      "eval:/" + typeNameToUrl(typeName)
    case CompiledScript(cs) =>
      cs.url
    case _ =>
      sourceName // keep it simple
  }

  private def typeNameToUrl(typeName: String): String = {
    typeName
      .replace("jdk.nashorn.internal.scripts.", "")
      .replace('.', '/')
      .replace('\\', '/')
      .replaceAll("[$^_]", "")
      .replaceFirst("/eval/?$", "")
  }
}
