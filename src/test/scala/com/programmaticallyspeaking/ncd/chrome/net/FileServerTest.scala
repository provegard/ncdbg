package com.programmaticallyspeaking.ncd.chrome.net

import java.io.{BufferedReader, File, InputStreamReader}
import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Scanner

import akka.actor.{ActorRef, ActorSystem}
import com.programmaticallyspeaking.ncd.chrome.domains.DomainFactory
import com.programmaticallyspeaking.ncd.testing.UnitTest
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

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

class FileServerServeTest extends UnitTest with ServerStarter[FileServer] with BeforeAndAfterEach with BeforeAndAfterAll {
  import scala.collection.JavaConverters._
  implicit val system = ActorSystem(getClass.getSimpleName)
  var server: FileServer = _
  var wrapperServer: WebSocketServer = _

  def requireContentType(file: File, ct: String) = {
    val url = server.publisher.publish(file)
    fetchHeaders(url.toString).map(_.get("Content-Type")) should be (Success(Some(List(ct))))
  }

  "FileServer" - {
    "and an existing file" - {
      val file = createTextFile("success")

      "doesn't serve the file before it has been published" in {
        fetchData(urlForFile(file)) should be ('failure)
      }

      "serves a file after it has been published" in {
        val url = server.publisher.publish(file)
        fetchData(url.toString) should be (Success("success"))
      }

      "doesn't serve a file outside the base URL" in {
        val url = server.publisher.publish(file).toString.replace("/files", "")
        fetchData(url) should be ('failure)
      }

      "uses the appropriate content type" in {
        requireContentType(file, "text/plain")
      }
    }

    "and a CoffeeScript file" - {
      val file = createTextFile("", ".coffee")

      "uses the appropriate content type" in {
        requireContentType(file, "application/vnd.coffeescript")
      }
    }

    "and a JavaScript file" - {
      val file = createTextFile("", ".js")

      "uses the appropriate content type" in {
        requireContentType(file, "application/javascript")
      }
    }

    "and a JavaScript file with capital extension" - {
      val file = createTextFile("", ".JS")

      "uses the appropriate content type" in {
        requireContentType(file, "application/javascript")
      }
    }

    "and an arbitrary file" - {
      val file = createTextFile("", ".bin")

      "uses the appropriate content type" in {
        requireContentType(file, "application/octet-stream")
      }
    }
  }

  def fetchData(url: String): Try[String] = Try {
    val conn = new URL(url).openConnection()
    val in = new BufferedReader(new InputStreamReader(conn.getInputStream, StandardCharsets.UTF_8))
    val s = new Scanner(in).useDelimiter("\\A")
    if (s.hasNext) s.next else ""
  }

  def fetchHeaders(url: String): Try[Map[String, List[String]]] = Try {
    val conn = new URL(url).openConnection()
    conn.getHeaderFields.asScala.map(e => e._1 -> e._2.asScala.toList).toMap
  }

  def urlForFile(f: File): String = {
    val pathPart = f.toURI.toString.replace("file:", "")
    server.baseURL + pathPart
  }

  def createTextFile(data: String, extension: String = ".txt"): File = {
    val file = File.createTempFile("test", extension)
    file.deleteOnExit()
    Files.write(file.toPath, data.getBytes(StandardCharsets.UTF_8))
    file
  }


  override protected def afterAll(): Unit = {
    system.terminate()
    super.afterAll()
  }

  override protected def beforeEach(): Unit = try {
    server = startServer()._1
  } finally super.beforeEach()

  override protected def afterEach(): Unit = try super.afterEach() finally {
    wrapperServer.stop()
  }

  override def startServer(port: Int): FileServer = {
    val server = new FileServer("localhost", port)
    // Ugh, this is ugly!!
    wrapperServer = new WebSocketServer(new FakeDomainFactory, Some(server))
    wrapperServer.start("localhost", port)
    server
  }

  class FakeDomainFactory extends DomainFactory {
    override def create(domain: String): ActorRef = throw new UnsupportedOperationException("Fake domain factory")
  }
}