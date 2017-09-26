package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.nashorn.NashornScriptHostTestFixture

import scala.concurrent.duration._
import scala.util.Try

object RunningJava9 {
  def java9Home: Option[String] = {
    if (isJava9) None
    else Option(System.getenv("JAVA9_HOME")).orElse(Some("c:\\Program files\\Java\\jdk-9"))
  }

  private lazy val isJava9 = {
    Try(Class.forName("jdk.nashorn.internal.runtime.Symbol")).isSuccess
  }
}

trait RunningJava9 { self: NashornScriptHostTestFixture =>
  // Java 9 on Travis is slow to start, increase the timeout a bit
  override val runVMTimeout: FiniteDuration = 16.seconds
  protected override def javaHome: Option[String] = RunningJava9.java9Home
}