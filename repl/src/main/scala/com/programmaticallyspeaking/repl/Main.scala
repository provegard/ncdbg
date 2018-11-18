package com.programmaticallyspeaking.repl

import java.io.{BufferedReader, InputStreamReader}

object Main extends App {
  println("Nashorn REPL")
  println("------------")
  println("Type :quit to quit.")

  val reader = new BufferedReader(new InputStreamReader(System.in))
  val repl = new Repl(reader)
  try repl.run() finally reader.close()
}