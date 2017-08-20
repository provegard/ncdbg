package com.programmaticallyspeaking.ncd.nashorn

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}

import scala.util.control.NonFatal

class StreamReadingThread(in: InputStream, appender: (String) => Unit) extends Thread {
  override def run(): Unit = {
    try {
      val reader = new BufferedReader(new InputStreamReader(in))
      var str = ""
      while (str != null) {
        str = reader.readLine()
        Option(str).foreach(appender)
      }
    } catch {
      case _: InterruptedException =>
      // ok
      case ex: IOException =>
        if (ex.getMessage != "Stream closed") ex.printStackTrace(System.err)
      case NonFatal(t) =>
        t.printStackTrace(System.err)
    }
  }
}
