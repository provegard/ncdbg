package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.Location

import scala.language.implicitConversions

object JDIExtensions {
  import scala.collection.JavaConverters._

  implicit def location2ExtLocation(l: Location): ExtLocation = new ExtLocation(l)

  class ExtLocation(location: Location) {

    lazy val byteCode: Int = {
      val methodByteCodes = location.method().bytecodes()
      val bc = methodByteCodes(location.codeIndex().toInt).toInt
      if (bc < 0) bc + 256 else bc
    }

    private val lineOfLastLocation = location.method().allLineLocations().asScala.last.lineNumber()
    val isLastLineInFunction: Boolean = location.lineNumber() == lineOfLastLocation
  }
}
