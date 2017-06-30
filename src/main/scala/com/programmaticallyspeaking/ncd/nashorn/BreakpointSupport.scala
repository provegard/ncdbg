package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.{Breakpoint, ScriptLocation}
import org.slf4s.Logging

trait BreakpointSupport { self: NashornDebuggerHost with Logging =>

  override def setBreakpoint(scriptUri: String, location: ScriptLocation, condition: Option[String]): Option[Breakpoint] = {
    findBreakableLocationsAtLine(scriptUri, location.lineNumber1Based) match {
      case Some(bls) =>
        // If we have a column number, find exactly that location. Otherwise grab all locations
        val candidates = location.columnNumber1Based match {
          case Some(col) => bls.filter(_.scriptLocation.columnNumber1Based.contains(col))
          case None => bls
        }
        if (candidates.nonEmpty) {
          val newId = breakpointIdGenerator.next
          val conditionDescription = condition.map(c => s" with condition ($c)").getOrElse("")
          log.info(s"Setting a breakpoint with ID $newId for location(s) ${candidates.mkString(", ")} in $scriptUri$conditionDescription")

          // Force boolean and handle that the condition contains a trailing comment
          val wrapper = condition.map(c =>
            s"""!!(function() {
               |return $c
               |})()
           """.stripMargin)

          val activeBp = ActiveBreakpoint(newId, candidates, wrapper)
          activeBp.enable()
          enabledBreakpoints += (activeBp.id -> activeBp)
          Some(activeBp.toBreakpoint)
        } else None

      case None =>
        log.trace(s"No breakable locations found for script $scriptUri at line ${location.lineNumber1Based}")
        None
    }
  }

  override def pauseOnBreakpoints(): Unit = {
    log.info("Will pause on breakpoints")
    willPauseOnBreakpoints = true
  }

  override def ignoreBreakpoints(): Unit = {
    log.info("Will ignore breakpoints")
    willPauseOnBreakpoints = false
  }

  override def getBreakpointLocations(scriptId: String, from: ScriptLocation, to: Option[ScriptLocation]): Seq[ScriptLocation] = {
    scriptById(scriptId).flatMap(script => breakableLocationsByScriptUrl.get(script.url.toString)) match {
      case Some(locations) =>
        // Get hold of all script locations we know of, but since Nashorn/Java doesn't report column number, we
        // a) ignore the column number
        // b) may end up with multiple ones with the same line number
        val candidates = locations.map(_.scriptLocation).filter { sloc =>
          sloc.lineNumber1Based >= from.lineNumber1Based && to.forall(sloc.lineNumber1Based < _.lineNumber1Based)
        }

        //TODO: Update doc
        // Filter so that we end up with one location per line, max. Since ScriptLocation is a case class and all
        // column numbers on the same line will be the same (again, since Nashorn/Java doesn't report column numbers),
        // it's sufficient to get the unique locations.
        candidates.distinct.sortBy(_.columnNumber1Based)

      case None => throw new IllegalArgumentException("Unknown script ID: " + scriptId)
    }
  }

}
