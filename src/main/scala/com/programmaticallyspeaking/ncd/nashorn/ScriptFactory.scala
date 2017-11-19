package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.Script
import com.programmaticallyspeaking.ncd.infra.{IdGenerator, ScriptURL}
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{EvaluatedCodeMarker, NoScriptReason, ScriptClassNamePrefix}
import com.sun.jdi.{Location, ReferenceType, ThreadReference, VirtualMachine}
import org.slf4s.Logging

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

case class IdentifiedScript(script: Script)

class ScriptFactory(virtualMachine: XVirtualMachine) extends Logging {
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
