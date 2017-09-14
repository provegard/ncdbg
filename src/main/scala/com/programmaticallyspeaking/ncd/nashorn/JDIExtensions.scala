package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ScopeType
import com.sun.jdi._

import scala.language.implicitConversions

object JDIExtensions {
  import scala.collection.JavaConverters._
  import TypeConstants._

  implicit def location2ExtLocation(l: Location): ExtLocation = new ExtLocation(l)
  implicit def value2ExtValue(v: Value): ExtValue = new ExtValue(v)
  implicit def refType2ExtRefType(refType: ReferenceType): ExtReferenceType = new ExtReferenceType(refType)

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
