package com.programmaticallyspeaking.ncd.testing

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.UnsynchronizedAppenderBase
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.status.ErrorStatus
import java.io.ByteArrayOutputStream
import java.io.IOException

import com.programmaticallyspeaking.ncd.messaging.{Observable, SerializedSubject}

object MemoryAppender {
  private[MemoryAppender] val logEventSubject = new SerializedSubject[ILoggingEvent]

  def logEvents: Observable[ILoggingEvent] = logEventSubject
}

class MemoryAppender extends UnsynchronizedAppenderBase[ILoggingEvent] {
  import MemoryAppender._
  protected var encoder: Encoder[ILoggingEvent] = _
  protected var outputStream = new ByteArrayOutputStream

  override def start(): Unit = {
    try {
      Option(encoder).foreach(_.init(outputStream))
      super.start()
    } catch {
      case e: IOException =>
        started = false
        addStatus(new ErrorStatus("Failed to initialize encoder for appender named [" + name + "].", this, e))
    }
  }

  override protected def append(event: ILoggingEvent): Unit = {
    if (!isStarted) return
    logEventSubject.onNext(event)
    try {
      event.prepareForDeferredProcessing()
      Option(encoder).foreach(_.doEncode(event))
    } catch {
      case ioe: IOException =>
        started = false
        addStatus(new ErrorStatus("IO failure in appender", this, ioe))
    }
  }

  def setEncoder(e: Encoder[ILoggingEvent]): Unit = {
    encoder = e
  }

  def getRenderedOutput = new String(outputStream.toByteArray)
}