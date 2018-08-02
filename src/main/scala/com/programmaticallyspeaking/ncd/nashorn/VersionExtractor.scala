package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.ReferenceType

import scala.util.Try

object VersionExtractor {
  def extract(referenceType: ReferenceType): Int =
    extract(referenceType.name())

  def extract(name: String): Int = {
    val parts = name.split('$')
    // Part 1 is: "jdk/nashorn/internal/scripts/Script"
    // Part 2 may be: "Recompilation" or compilation ID, but only if ID > 0
    // Part 3 is compilation ID if part 2 is "Recompilation"
    val compilationIdPart = if (parts.lift(1).contains("Recompilation")) 2 else 1
    // Wrap in Try to handle missing ID, e.g.: jdk.nashorn.internal.scripts.Script$\^eval\_
    Try(parts.lift(compilationIdPart).map(_.toInt).getOrElse(0)).getOrElse(0)
  }
}
