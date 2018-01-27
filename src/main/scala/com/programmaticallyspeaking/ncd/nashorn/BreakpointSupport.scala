package com.programmaticallyspeaking.ncd.nashorn

import java.util.concurrent.ConcurrentHashMap

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.messaging.{Observer, Subscription}
import org.slf4s.Logging

/**
  * Tracks requested breakpoints. When we see a new script with an URL that matches an existing breakpoint,
  * we need to add new locations to it. This makes it possible to set a breakpoint in a script that hasn't been
  * loaded yet.
  *
  * @param scriptIdentity script identity
  * @param lineBase1 line number
  * @param activeBreakpointId the identity of the breakpoint resulting from the request
  */
case class RequestedBreakpoint(scriptIdentity: ScriptIdentity, lineBase1: Int, activeBreakpointId: String)

trait BreakpointSupport { self: NashornDebuggerHost with Logging =>
  import scala.collection.JavaConverters._
  private val requestedBreakpoints = new ConcurrentHashMap[String, RequestedBreakpoint]()
  private var _scriptAddedSubscription: Subscription = _

  private def setupBreakpointResolution(): Unit = {
    //TODO: I think it would be nice to do this earlier, like on init time. But the trait solution doesn't really
    //TODO: allow that, does it?
    if (_scriptAddedSubscription == null) {
      _scriptAddedSubscription = events.subscribe(Observer.from[ScriptEvent] {
        case ScriptAdded(script) =>
          requestedBreakpoints.values().asScala.filter(_.scriptIdentity.matchesScript(script)).foreach { rb =>

            val newBls = findBreakableLocationsAtLine(rb.scriptIdentity, rb.lineBase1)
            _breakpoints.addBreakableLocations(rb.activeBreakpointId, newBls)

            emitEvent(BreakpointResolved(rb.activeBreakpointId, LocationInScript(script.id, ScriptLocation(rb.lineBase1, None))))
          }
        case _ => //TODO: Get rid of the need for this
      })
    }
  }

  private def recordBreakpointRequests(identity: ScriptIdentity, location: ScriptLocation, activeBreakpoint: ActiveBreakpoint): Unit = {
    identity match {
      case IdBasedScriptIdentity(_) => // no action
      case other =>
        // Record the desire to set a breakpoint, because we have two cases here:
        // 1. It's a URL, in which case the user may have used DevTools Workspace to set a breakpoint in a file
        //    that hasn't been seen as a script yet.
        // 2. It's a URL regexp, in which case future scripts may match.
        requestedBreakpoints.put(activeBreakpoint.id, RequestedBreakpoint(other, location.lineNumber1Based, activeBreakpoint.id))
    }
  }

  override def removeBreakpointById(id: String): Unit = {
    //TODO: Figure out an appropriate way to test removal from requestedBreakpoints here
    requestedBreakpoints.remove(id)
    _breakpoints.disableById(id)
  }

  override def setBreakpoint(id: ScriptIdentity, location: ScriptLocation, condition: Option[String]): Breakpoint = {
    setupBreakpointResolution()

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

    val conditionDescription = condition.map(c => s" with condition ($c)").getOrElse("")

    // Force boolean and handle that the condition contains a trailing comment
    val wrapper = condition.map(c =>
      s"""!!(function() {
         |return $c
         |})()
     """.stripMargin)

    val activeBp = _breakpoints.create(candidates, wrapper)
    log.info(s"Setting a breakpoint with ID ${activeBp.id} for location(s) ${candidates.mkString(", ")} in $id$conditionDescription")
    recordBreakpointRequests(id, location, activeBp)
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
