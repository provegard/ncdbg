package com.programmaticallyspeaking.repl

import java.io.BufferedReader

import javax.script.ScriptException

import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Repl(reader: BufferedReader) {
  private val engine = new Engine
  private val heldLines = ListBuffer[String]()

  def run(): Unit = {
    loopAround()
  }

  private def loopAround(): Unit = {
    var done = false

    while (!done) {
      showPrompt
      val line = reader.readLine()
      if (line == ":quit") {
        done = true
      } else {
        evaluateLine(line)
      }
    }
  }

  private def evaluateLine(line: String): Unit = {
    val script = (heldLines :+ line).mkString("\n")
    engine.evaluate(script) match {
      case Success(result) =>
        heldLines.clear()
        println(result)

      case Failure(s: ScriptException) if continuationRequired(s) =>
        heldLines += line

      case Failure(NonFatal(t)) =>
        t.printStackTrace(System.out)
        println("")
    }
  }

  private def continuationRequired(s: ScriptException): Boolean = {
    val msg = s.getMessage
    // E.g.: javax.script.ScriptException: <eval>:1:12 Expected } but found eof
    msg.contains("but found eof")
  }

  private def showPrompt = {
    val prompt = if (heldLines.isEmpty) "> " else "| "
    print(prompt)
    System.out.flush()
  }
}
