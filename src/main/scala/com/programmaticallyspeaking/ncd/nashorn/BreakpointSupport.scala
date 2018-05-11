package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.Observer
import org.slf4s.Logging

trait BreakpointSupport { self: NashornDebuggerHost with Logging =>

  protected def initBreakpointSupport(): Unit = {
    _breakpoints.resolvedBreakpoints.subscribe(Observer.from[BreakpointResolved] {
      case br: BreakpointResolved => emitEvent(br)
    })
  }

  override def removeBreakpointById(id: String): Unit = {
    _breakpoints.removeById(id)
  }

  override def setBreakpoint(id: ScriptIdentity, location: ScriptLocation, options: BreakpointOptions): Breakpoint = {
    val bls = findBreakableLocationsAtLine(id, location.lineNumber1Based)
    // If we have a column number, try to find exactly that location, but fall back to locations on the line.
    // The reason is that column numbers is not an exact science, especially when it comes to source maps.
    val candidates = location.columnNumber1Based match {
      case Some(col) => bls.filter(_.scriptLocation.columnNumber1Based.contains(col)).toList match {
        case Nil => bls
        case xs => xs
      }
      case None => bls
    }

    val conditionDescription = options.condition.map(c => s" with condition ($c)").getOrElse("")

    // Force boolean and handle that the condition contains a trailing comment
    val wrapper = options.condition.map(c =>
      s"""!!(function() {
         |return $c
         |})()
     """.stripMargin)

    val activeBp = _breakpoints.create(id, location, candidates, wrapper, options.oneOff)
    if (activeBp.isUnresolved)
      log.info(s"Setting an unresolved breakpoint with ID ${activeBp.id} at line ${location.lineNumber1Based} in $id$conditionDescription")
    else
      log.info(s"Setting a breakpoint with ID ${activeBp.id} for location(s) ${candidates.mkString(", ")} in $id$conditionDescription")
    activeBp.toBreakpoint
  }

  override def pauseOnBreakpoints(): Unit = {
    log.info("Will pause on breakpoints")
    _pauser.pauseOnBreakpoints(true)
  }

  override def ignoreBreakpoints(): Unit = {
    log.info("Will ignore breakpoints")
    _pauser.pauseOnBreakpoints(false)
  }

  override def getBreakpointLocations(id: ScriptIdentity, from: ScriptLocation, to: Option[ScriptLocation]): Seq[ScriptLocation] = {
    _breakableLocations.byScriptIdentity(id) match {
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

      case None => throw new IllegalArgumentException("Unknown script ID: " + id)
    }
  }

}
