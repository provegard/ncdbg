package com.programmaticallyspeaking.ncd.nashorn

import java.io.BufferedReader

trait ScriptExecutorBase {
  val reader: BufferedReader

  protected def readStdin(): String = reader.readLine()
  protected def waitForSignal(expected: String): Unit = {
    println(s"Awaiting '$expected' signal")
    val signal = readStdin()
    if (signal != expected) {
      println(s"Didn't get '$expected' signal, got: " + signal)
      System.exit(1)
    }
  }
}
