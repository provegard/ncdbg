package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.nashorn.NashornScriptHostTestFixture

import scala.concurrent.duration._

object RunningJava9 {
  val java9Home = Option(System.getenv("JAVA9_HOME")).getOrElse("c:\\Program files\\Java\\jdk-9")
}

trait RunningJava9 { self: NashornScriptHostTestFixture =>
  // Java 9 on Travis is slow to start, increase the timeout a bit
  override val runVMTimeout: FiniteDuration = 16.seconds
  protected override def javaHome: Option[String] = Some(RunningJava9.java9Home)
}