package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.nashorn.NashornScriptHostTestFixture

object RunningJava9 {
  val java9Home = Option(System.getenv("JAVA9_HOME")).getOrElse("c:\\Program files\\Java\\jdk-9")
}

trait RunningJava9 { self: NashornScriptHostTestFixture =>
  protected override def javaHome: Option[String] = Some(RunningJava9.java9Home)
}