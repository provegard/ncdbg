package com.programmaticallyspeaking.ncd.nashorn.java9

import com.programmaticallyspeaking.ncd.nashorn.NashornScriptHostTestFixture

trait RunningJava9 { self: NashornScriptHostTestFixture =>
  protected override def javaHome: Option[String] = Some("c:\\Program files\\Java\\jdk-9")
}