package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.ErrorValue
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.StackFrameHolder
import com.sun.jdi.Location
import com.sun.jdi.event.ExceptionEvent

private[nashorn] class ExceptionEventInfo(event: ExceptionEvent, stackFrames: Seq[StackFrameHolder], marshaller: Marshaller) {
  import NashornDebuggerHost._
  private val exception = event.exception()
  val exceptionTypeName = exception.referenceType().name()
  val isECMAException = exceptionTypeName == NIR_ECMAException
  val exceptionType = ExceptionType.determine(event.catchLocation(), stackFrames)
  def throwLocation: Location = stackFrames.headOption.map(_.location).getOrElse(throw new IllegalStateException("Empty stack"))
  def marshalledException: Either[String, ErrorValue] = marshaller.marshal(exception) match {
    case err: ErrorValue => Right(err)
    case other => Left(s"Exception $exceptionTypeName marshalled to unexpected: $other")
  }
}
