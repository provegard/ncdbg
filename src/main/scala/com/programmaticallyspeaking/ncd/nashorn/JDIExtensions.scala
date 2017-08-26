package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ScopeType
import com.sun.jdi.{Location, Value}

import scala.language.implicitConversions

object JDIExtensions {
  import scala.collection.JavaConverters._

  implicit def location2ExtLocation(l: Location): ExtLocation = new ExtLocation(l)
  implicit def value2ExtValue(v: Value): ExtValue = new ExtValue(v)

  class ExtLocation(location: Location) {

    lazy val byteCode: Int = {
      val methodByteCodes = location.method().bytecodes()
      val bc = methodByteCodes(location.codeIndex().toInt).toInt
      if (bc < 0) bc + 256 else bc
    }

    private val lineOfLastLocation = location.method().allLineLocations().asScala.last.lineNumber()
    val isLastLineInFunction: Boolean = location.lineNumber() == lineOfLastLocation
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
}
