package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ScriptVersion
import com.sun.jdi.ReferenceType

import scala.util.Try

object VersionExtractor {
  def extract(referenceType: ReferenceType): ScriptVersion =
    extract(referenceType.name())

  def extract(name: String): ScriptVersion = {
    val parts = name.split('$')
    // Part 1 is: "jdk/nashorn/internal/scripts/Script"
    // Part 2 may be: "Recompilation" or compilation ID, but only if ID > 0
    // Part 3 is compilation ID if part 2 is "Recompilation"
    val isRecompilation = parts.lift(1).contains("Recompilation")
    val compilationIdPart = if (isRecompilation) 2 else 1
    // Wrap in Try to handle missing ID, e.g.: jdk.nashorn.internal.scripts.Script$\^eval\_
    val compilationId = Try(parts.lift(compilationIdPart).map(_.toInt).getOrElse(0)).getOrElse(0)
    ScriptVersion(compilationId, !isRecompilation)
  }
}
