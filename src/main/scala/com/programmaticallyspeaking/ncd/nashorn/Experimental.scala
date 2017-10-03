package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.infra.{IdGenerator, PathUtils, ScriptURL}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{EvaluatedCodeMarker, NoScriptReason, ScriptClassNamePrefix}
import com.sun.jdi._
import org.slf4s.Logging

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

class Scripts {
  private val scriptIdGenerator = new IdGenerator("ndx") // special prefix for replacement scripts

  private var _scripts = Map[ScriptURL, Script]()

  def scripts: Seq[Script] = _scripts.values.groupBy(_.id).flatMap(_._2.headOption).toSeq

  def suggest(script: Script): Script = {
    require(byId(ScriptIdentity.fromId(script.id)).isEmpty, s"Script with ID ${script.id} has already been added")
    // For a recompilation, we will (most likely) already have the original script that was recompiled (recompilation
    // happens for example when a function inside the eval script is called with known types). We find the original
    // script by comparing contents hashes. If we find the original script, we just discard the new one and use the
    // original.
    _scripts.values.find(_.contentsHash() == script.contentsHash()) match {
      case Some(scriptWithSameSource) =>
        // Note that we add a map entry for the original script with the new URL as key. This way we'll find our
        // reused script using all its "alias URLs".
        // Note 2: I worry that comparing contents hashes isn't enough - that we need to verify no overlapping
        // line locations also. But we don't have locations here, and I don't want to do too much defensive coding.
        _scripts += script.url -> scriptWithSameSource
        scriptWithSameSource
      case None =>
        _scripts.get(script.url) match {
          case Some(_) =>
            // This is a new script with new contents but with the same URL as an old one - it is likely a script
            // that has been reloaded via Nashorn's 'load' function.
            // The choice of using the ID as suffix is pretty arbitrary - it could also be a sequence number.
            val newId = scriptIdGenerator.next
            //TODO: Don't use PathUtils here, it's URL now
            val newURLString = PathUtils.insertNameSuffix(script.url.toString, "_" + newId)
            val newURL = ScriptURL.create(newURLString)

            val replacement = ScriptImpl.fromSource(newURL, script.contents, newId)
            _scripts += newURL -> replacement
            replacement

          case None =>
            _scripts += script.url -> script
            script
        }
    }
  }

  def byId(id: ScriptIdentity): Option[Script] = {
    id match {
      case IdBasedScriptIdentity(x) =>
        _scripts.values.find(_.id == x)
      case URLBasedScriptIdentity(url) =>
        _scripts.get(ScriptURL.create(url))
    }
  }
}

class BreakableLocations(virtualMachine: VirtualMachine, scripts: Scripts) extends Logging {

  import JDIExtensions._

  private var breakableLocationsByScriptUrl = Map[String, Seq[BreakableLocation]]()
//  private var scriptUrlById = Map[String, String]()

  def add(script: Script, locations: Seq[Location]): Unit = {
    add0(script, gatherBreakableLocations(script, locations))
  }

  private def add0(script: Script, breakableLocations: Seq[BreakableLocation]): Unit = {
    val existing = breakableLocationsByScriptUrl.getOrElse(script.url.toString, Seq.empty)

    // Remove existing ones where Location is unset and the line number exists among the new ones. These are placeholders
    // added so that it's possible to set a breakpoint in a function before it has been executed, but now we have real
    // locations for that function (supposedly).
    // TODO: This may be broken if multiple one-liner functions are defined on the same line...
    val lineNumbersOfNewOnes = breakableLocations.map(_.scriptLocation.lineNumber1Based).toSet

    // TODO: Identify BLs no longer relevant due to recompilation. Perhaps by function node ID? If such a BL
    // TODO: is enabled, it needs to be disabled.
    def isObsolete(bl: BreakableLocation) = bl.isPlaceholder && lineNumbersOfNewOnes.contains(bl.scriptLocation.lineNumber1Based)

    val lineNumbersOfEnabled = existing.filter(_.isEnabled).map(_.scriptLocation.lineNumber1Based).toSet

    // Auto-enable a BL if we have an existing one that is enabled for the same line number - regardless of whether
    // it's a placeholder or not. This is untested for non-placeholders, since we probably need a big test script
    // to trigger this case.
    def shouldEnable(bl: BreakableLocation) = lineNumbersOfEnabled.contains(bl.scriptLocation.lineNumber1Based)

    breakableLocations.filter(shouldEnable).foreach { bl =>
      log.debug(s"Auto-enabling breakable location $bl since it's on the same line as a currently enabled one.")
      bl.enable()
    }

    val newList = existing.filterNot(isObsolete) ++ breakableLocations
    breakableLocationsByScriptUrl += script.url.toString -> newList
  }

  def byScriptIdentity(id: ScriptIdentity): Option[Seq[BreakableLocation]] =
    findScriptUrl(id).flatMap(breakableLocationsByScriptUrl.get)

  def byLocation(location: Location): Option[BreakableLocation] = {
    val url = location.scriptURL
    // There may be multiple breakable locations for the same line (even in the same method - e.g. for a 'while'
    // statement that is last in a method). Try to find an exact match first, then fall back to finding a location
    // on the correct line. The fallback is necessary since the passed-in Location may have a code index that is
    // different from the one stored in a BreakableLocation.
    val id = ScriptIdentity.fromURL(url)
    atLine(id, location.lineNumber()).flatMap { bls =>
      bls.find(_.hasLocation(location)).orElse(bls.find(_.sameMethodAndLineAs(location)))
    }
  }

  def atLine(id: ScriptIdentity, lineNumber: Int): Option[Seq[BreakableLocation]] = findScriptUrl(id) match {
    case Some(scriptUrl) =>
      breakableLocationsByScriptUrl.get(scriptUrl).map { breakableLocations =>
        breakableLocations.filter(_.scriptLocation.lineNumber1Based == lineNumber)
      }

    case None =>
      throw new IllegalArgumentException("Unknown script: " + id)
  }

  private def findScriptUrl(id: ScriptIdentity): Option[String] = scripts.byId(id).map(_.url.toString)

  private def potentiallyBreakableLines(script: Script): Seq[Int] = {
    def hasRelevantContent(line: String) = {
      val trimmed = line.trim()
      trimmed != "" && trimmed != "{" && trimmed != "}"
    }
    def looksBreakable(line: Int) = script.sourceLine(line).exists(hasRelevantContent)
    (1 to script.lineCount).filter(looksBreakable)
  }

  private def gatherBreakableLocations(script: Script, locations: Seq[Location]): Seq[BreakableLocation] = {
    val erm = virtualMachine.eventRequestManager()
    // Find all potentially breakable lines. Create breakable locations from actual locations. Then create
    // candidates for the potentially breakable lines that weren't covered. Such a line may for example belong to
    // a function that hasn't been executed yet.
    var lineNumbers = potentiallyBreakableLines(script).toSet
    val breakableLocations = locations.map(l => new BreakableLocation(script, erm, l))
    breakableLocations.foreach(bl => lineNumbers -= bl.scriptLocation.lineNumber1Based)
    breakableLocations ++ lineNumbers.map(line => new BreakableLocation(script, erm, line))
  }

}

case class IdentifiedScript(script: Script)

class ScriptFactory(virtualMachine: VirtualMachine) extends Logging {
  import JDIExtensions._

  import scala.collection.JavaConverters._

  private val scriptIdGenerator = new IdGenerator("nds")

  def considerReferenceType(thread: Option[ThreadReference], refType: ReferenceType): Future[Option[IdentifiedScript]] = {
    val className = refType.name()

    if (className.startsWith(ScriptClassNamePrefix)) {
      // This is a compiled Nashorn script class.
      log.debug(s"Found a script reference type: ${refType.name}")

      Try(refType.allLineLocations().asScala) match {
        case Success(locations) =>
          locations.headOption match {
            case Some(firstLocation) =>
              // Note that we no longer try to use the script path for reading the source. If the script contains a
              // sourceURL annotation, Nashorn will use that at script path, so we might end up reading CoffeeScript
              // source instead of the real source.
              val scriptURL = firstLocation.scriptURL
              val triedScript = Try(scriptFromEval(refType, scriptURL))
              handleScriptResult(thread, triedScript, refType, locations)

            case None =>
              log.debug(s"Ignoring script type '${refType.name} because it has no line locations.")
              Future.successful(None)
          }
        case Failure(t) =>
          log.warn(s"Failed to get line locations for ${refType.name}", t)
          Future.successful(None)
      }
    } else Future.successful(None)
  }

  private def handleScriptResult(maybeThread: Option[ThreadReference], result: Try[Either[NoScriptReason.EnumVal, Script]],
                                 refType: ReferenceType, locations: Seq[Location]): Future[Option[IdentifiedScript]] = result match {
    case Success(Right(script)) =>
      Future.successful(Some(identifiedScript(script, locations)))
    case Success(Left(NoScriptReason.EvaluatedCode)) =>
      log.debug(s"Ignoring script because it contains evaluated code")
      Future.successful(None)
    case Success(Left(NoScriptReason.NoSource)) =>
      val installPhaseType =
        maybeThread.flatMap(t => t.frames.asScala.view.map(f => f.location().method().declaringType()).find(_.name().endsWith("$InstallPhase")))
      installPhaseType match {
        case Some(ciType) =>
          extractSourceLater(maybeThread, refType, ciType)
        case None =>
          log.warn(s"Cannot get source from ${refType.name()}.")
          Future.successful(None)
      }
    case Failure(t) =>
      log.error(s"Ignoring script type $refType", t)
      Future.successful(None)
  }

  private def extractSourceLater(maybeThread: Option[ThreadReference], refType: ReferenceType, ciType: ReferenceType): Future[Option[IdentifiedScript]] = {
    log.debug(s"Will get source from ${refType.name()} when InstallPhase is complete")
    val promise = Promise[Option[IdentifiedScript]]()
    // Let the stack unwind a bit in order to get hold of the source. This logic is based on the observation that
    // we see the ClassPrepareEvent event inside Context$ContextCodeInstaller (in Java 9, one of its subclasses).
    // In Java 8, it's the initialize method, in Java 9 it's the install method. The source isn't set until in
    // the initialize method, so the safest approach is to wait until the entire install phase (InstallPhase type)
    // is complete.
    val req = virtualMachine.eventRequestManager().createMethodExitRequest()
    req.addClassFilter(ciType)
    req.addThreadFilter(maybeThread.get) // .get is safe here, since installPhaseType is defined
    req.addCountFilter(1)
    req.onEventDo { _ =>
      log.debug(s"Getting source from ${refType.name()} at method exit from InstallPhase")
      virtualMachine.eventRequestManager().deleteEventRequest(req) // not sure if this is needed
      promise.completeWith(considerReferenceType(None, refType))
      true // consume the event
    }
    req.setEnabled(true)
    promise.future
  }

  private def identifiedScript(script: Script, locations: Seq[Location]): IdentifiedScript = {
    IdentifiedScript(script)
  }

  private def scriptFromEval(refType: ReferenceType, scriptURL: ScriptURL): Either[NoScriptReason.EnumVal, Script] = {
    refType.shamelesslyExtractEvalSourceFromPrivatePlaces().map { src =>
      if (src.contains(EvaluatedCodeMarker)) Left(NoScriptReason.EvaluatedCode)
      else Right(newScript(scriptURL, src))
    }.getOrElse(Left(NoScriptReason.NoSource))
  }

  private def newScript(url: ScriptURL, source: String) =
    ScriptImpl.fromSource(url, source, scriptIdGenerator.next)
}
