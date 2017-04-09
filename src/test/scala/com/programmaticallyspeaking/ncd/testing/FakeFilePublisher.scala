package com.programmaticallyspeaking.ncd.testing

import java.io.File
import java.net.URL

import com.programmaticallyspeaking.ncd.chrome.net.FilePublisher

object FakeFilePublisher extends FilePublisher {
  override def publish(file: File): URL = new URL("http://localhost/no/such/file")
}
