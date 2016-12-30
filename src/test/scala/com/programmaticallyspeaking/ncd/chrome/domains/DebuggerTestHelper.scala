package com.programmaticallyspeaking.ncd.chrome.domains

// DON'T TOUCH THIS FILE - LINE NUMBERS ARE IMPORTANT!
object DebuggerTestHelper {
  def fail[R](msg: String): R = {
    throw new Exception(msg)
  }
}
