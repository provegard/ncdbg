package com.programmaticallyspeaking.ncd.chrome.net

import java.io.{BufferedReader, File, InputStreamReader}
import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Scanner

import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.BeforeAndAfterEach

import scala.util.{Success, Try}

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
}

class FileServerServeTest extends UnitTest with ServerStarter[FileServer] with BeforeAndAfterEach {

  var server: FileServer = _

  "FileServer" - {
    "and an existing file" - {
      val file = createTextFile("success")

      "doesn't serve the file before it has been published" in {
        fetch(urlForFile(file)) should be ('failure)
      }

      "serves a file after it has been published" in {
        val url = server.publisher.publish(file)
        fetch(url.toString) should be (Success("success"))
      }
    }
  }

  def fetch(url: String): Try[String] = Try {
    val conn = new URL(url).openConnection()
    val in = new BufferedReader(new InputStreamReader(conn.getInputStream, StandardCharsets.UTF_8))
    val s = new Scanner(in).useDelimiter("\\A")
    if (s.hasNext) s.next else ""
  }

  def urlForFile(f: File): String = {
    val pathPart = f.toURI.toString.replace("file:", "")
    server.baseURL + pathPart
  }

  def createTextFile(data: String): File = {
    val file = File.createTempFile("test", ".txt")
    file.deleteOnExit()
    Files.write(file.toPath, data.getBytes(StandardCharsets.UTF_8))
    file
  }

  override protected def beforeEach(): Unit = try {
    server = startServer()._1
  } finally super.beforeEach()

  override protected def afterEach(): Unit = try super.afterEach() finally {
    server.stop()
  }

  override def startServer(port: Int): FileServer = {
    val server = new FileServer("localhost", port)
    server.start()
    server
  }
}