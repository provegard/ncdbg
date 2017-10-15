package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.StackFrameHolder
import com.sun.jdi.Location

object ExceptionType {
  sealed trait EnumVal
  case object CaughtByScript extends EnumVal { override def toString = "caught" }
  case object UncaughtByScript extends EnumVal { override def toString = "uncaught" }
  case object Unknown extends EnumVal { override def toString = "unknown" }

  def determine(catchLocation: Location, stackFrames: Seq[StackFrameHolder]): EnumVal = {
    catchLocation match {
      case loc if loc == null => UncaughtByScript
      case loc =>
        val catchMethod = loc.method()
        def isNotCatchFrame(sf: StackFrameHolder) = sf.location.method() != catchMethod
        val framesFromCatchLocation = stackFrames.span(isNotCatchFrame)._2.toList
        framesFromCatchLocation match {
          case x :: _ if x.belongsToScript => CaughtByScript
          case _ :: rest =>
            // Catch location is a non-script. If there are no script frames beyond the catch location, then the
            // exception is uncaught. Otherwise, it's impossible to know (since we cannot get the exception table
            // via JDI).
            val hasScriptFrameAfterCatchLocation = rest.exists(_.belongsToScript)
            if (hasScriptFrameAfterCatchLocation) Unknown else UncaughtByScript
          case Nil =>
            // Couldn't find catch frame...
            Unknown
        }
    }
  }
}
