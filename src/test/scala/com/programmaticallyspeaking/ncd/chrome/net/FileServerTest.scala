package com.programmaticallyspeaking.ncd.chrome.net

import java.io.File

import com.programmaticallyspeaking.ncd.testing.UnitTest

class FileServerTest extends UnitTest {

  "FileServer" - {
    "publisher" - {
      val server = new FileServer("localhost", 8080)
      val publisher = server.publisher

      "publishes a file and returns the URL" in {
        val file = new File("/tmp/foo.txt")
        val url = publisher.publish(file)
        url.toString should be ("http://localhost:8080/files" + file.toURI.toString.replace("file:", ""))
      }

      "doesn't use two slashes between the base URL and the file path" in {
        val url = publisher.publish(new File("/tmp/foo.txt"))
        url.toString should not include ("files//")
      }

    }
  }

  //TODO: Test actual serving of files
}
