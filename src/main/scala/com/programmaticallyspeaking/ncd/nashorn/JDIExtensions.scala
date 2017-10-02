package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ScopeType
import com.programmaticallyspeaking.ncd.infra.ScriptURL
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{EventHandler, EventHandlerKey}
import com.sun.jdi._
import com.sun.jdi.event.Event
import com.sun.jdi.request.EventRequest

import scala.language.implicitConversions

object JDIExtensions {
  import scala.collection.JavaConverters._
  import TypeConstants._

  implicit def location2ExtLocation(l: Location): ExtLocation = new ExtLocation(l)
  implicit def value2ExtValue(v: Value): ExtValue = new ExtValue(v)
  implicit def refType2ExtRefType(refType: ReferenceType): ExtReferenceType = new ExtReferenceType(refType)
  implicit def eventRequest2ExtRequest(eventRequest: EventRequest): ExtRequest = new ExtRequest(eventRequest)
  implicit def event2ExtEvent(event: Event): ExtEvent = new ExtEvent(event)

  class ExtEvent(event: Event) {
    def handle(): Option[Boolean] = {
      Option(event.request().getProperty(EventHandlerKey)).collect {
        case h: EventHandler => h(event)
      }
    }
  }

  class ExtRequest(eventRequest: EventRequest) {
    def onEventDo(h: EventHandler): Unit = {
      Option(eventRequest.getProperty(EventHandlerKey)).foreach(_ => throw new IllegalStateException("Event handler already associated."))
      eventRequest.putProperty(EventHandlerKey, h)
    }
  }

  class ExtLocation(location: Location) {

    /**
      * Determines if the location is in ScriptRuntime.DEBUGGER.
      */
    def isDebuggerStatement: Boolean =
      location.declaringType().name() == NIR_ScriptRuntime && location.method().name() == ScriptRuntime_DEBUGGER


    lazy val byteCode: Int = {
      val methodByteCodes = location.method().bytecodes()
      val bc = methodByteCodes(location.codeIndex().toInt).toInt
      if (bc < 0) bc + 256 else bc
    }

    private lazy val lineOfLastLocation = location.method().allLineLocations().asScala.last.lineNumber()
    def isLastLineInFunction: Boolean = location.lineNumber() == lineOfLastLocation

    def sameMethodAndLineAs(other: Option[Location]): Boolean =
      other.exists(l => l.method() == location.method() && l.lineNumber() == location.lineNumber())

    def scriptURL: ScriptURL = ScriptURL.create(scriptPath)
    private lazy val scriptPath: String = {
      // It appears *name* is a path on the form 'file:/c:/...', whereas path has a namespace prefix
      // (jdk\nashorn\internal\scripts\). This seems to be consistent with the documentation (although it's a bit
      // surprising), where it is stated that the Java stratum doesn't use source paths and a path therefore is a
      // package-qualified file name in path form, whereas name is the unqualified file name (e.g.:
      // java\lang\Thread.java vs Thread.java).
      val path = location.sourceName()
      if (path == "<eval>") {
        // For evaluated scripts, convert the type name into something that resembles a file URI.
        val typeName = location.declaringType().name()
        "eval:/" + typeName
          .replace("jdk.nashorn.internal.scripts.", "")
          .replace('.', '/')
          .replace('\\', '/')
          .replaceAll("[$^_]", "")
          .replaceFirst("/eval/?$", "")
      } else {
        path // keep it simple
      }
    }

  }

  class ExtValue(v: Value) {
    lazy val scopeType: ScopeType = {
      val typeName = v.`type`().name()
      // jdk.nashorn.internal.objects.Global
      if (typeName.endsWith(".Global"))
        ScopeType.Global
      // jdk.nashorn.internal.runtime.WithObject
      else if (typeName.endsWith(".WithObject"))
        ScopeType.With
      else ScopeType.Closure
    }

  }

  class ExtReferenceType(referenceType: ReferenceType) {
    private def scriptSourceField(refType: ReferenceType): Field = {
      // Generated script classes has a field named 'source'
      Option(refType.fieldByName("source"))
        .getOrElse(throw new Exception("Found no 'source' field in " + refType.name()))
    }

    /**
      * This method extracts the source code of an evaluated script, i.e. a script that hasn't been loaded from a file
      * and therefore doesn't have a path/URL. The official way to do this would be to call `DebuggerSupport.getSourceInfo`,
      * but we cannot do that because when we connect to the VM and discover all scripts, we don't have a thread that is
      * in the appropriate state to allow execution of methods. However, extracting field values is fine, so we dive deep
      * down in the Nashorn internals to grab the raw source code data.
      *
      * @return a source code string
      */
    def shamelesslyExtractEvalSourceFromPrivatePlaces(): Option[String] = {
      val sourceField = scriptSourceField(referenceType)
      // Get the Source instance in that field
      Option(referenceType.getValue(sourceField).asInstanceOf[ObjectReference]).map { source =>
        // From the instance, get the 'data' field, which is of type Source.Data
        val dataField = Option(source.referenceType().fieldByName("data"))
          .getOrElse(throw new Exception("Found no 'data' field in " + source.referenceType().name()))
        // Get the Source.Data instance, which should be a RawData instance
        val data = source.getValue(dataField).asInstanceOf[ObjectReference]
        // Source.RawData has a field 'array' of type char[]
        val charArrayField = Option(data.referenceType().fieldByName("array"))
          .getOrElse(throw new Exception("Found no 'array' field in " + data.referenceType().name()))
        // Get the char[] data
        val charData = data.getValue(charArrayField).asInstanceOf[ArrayReference]
        // Get individual char values from the array
        val chars = charData.getValues.asScala.map(v => v.asInstanceOf[CharValue].charValue())
        // Finally combine into a string
        chars.mkString
      }
    }

  }
}
