package com.programmaticallyspeaking.ncd.chrome.net

import java.io._
import java.net.{URI, URL}
import java.nio.channels.Channels
import java.nio.charset.{Charset, StandardCharsets}
import java.util.concurrent.ConcurrentHashMap

import com.programmaticallyspeaking.tinyws.Server
import org.slf4s.Logging

import scala.util.control.NonFatal

trait FilePublisher {
  /**
    * Publishes a file and returns the URL under which it has been published.
    *
    * @param file the file to publish
    * @return an URL
    */
  def publish(file: File): URL
}

/**
  * A file server implemented as a fallback handler for tinyws, which means that an instance of this class cannot
  * be "started" or "stopped". Name and host arguments are only used for creating the base URL for serving files.
  *
  * @param host the name of host that the server runs on
  * @param port the port that the server listens to
  */
class FileServer(host: String, port: Int) extends Logging with Server.FallbackHandler {
  import scala.collection.JavaConverters._
  import FileServer._
  /**
    * The base URL for files, not including a trailing slash.
    */
  val baseURL: String = s"http://$host:$port/files"

  private val baseURI = new URI(baseURL)

  private val fileWhitelist = ConcurrentHashMap.newKeySet[File]()

  private def isOkToServe(file: File): Boolean = fileWhitelist.contains(file)

  val publisher: FilePublisher = (file: File) => {
    fileWhitelist.add(file)
    val path = file.toURI.toString.replace("file:", "")
    val result = baseURL + path

    log.info(s"Whitelisting $file for the file server, publishing under $result")
    new URL(result)
  }

  override def handle(connection: Server.Connection): Unit = {
    try serveFile(connection) catch {
      case _: FileNotFoundException =>
        connection.sendResponse(404, "Not Found", null)
      case ex: UnsupportedOperationException =>
        connection.sendResponse(405, "Method Not Allowed", Map("Allow" -> ex.getMessage).asJava)
      case NonFatal(t) =>
        log.error("File server error", t)
        connection.sendResponse(500, "Internal Server Error", null)
    }
    connection.outputStream().close()
  }

  private def serveFile(connection: Server.Connection): Unit = {
    // The requested path must start with our base path (+ a slash)
    val path = connection.uri().getPath
    if (path.startsWith(baseURI.getPath + "/")) {
      val path = baseURI.relativize(connection.uri()).getPath
      val method = connection.method()
      if (method == "GET" || method == "HEAD") {
        log.info(s"Request ($method) for file with path $path")
        val thePath = if (path.startsWith("/")) path else "/" + path
        val fileURI = new URI("file:" + thePath)
        val file = new File(fileURI)
        try serveFile(connection, file) catch {
          case ex: FileNotFoundException =>
            log.warn(s"File at $path is not whitelisted")
            throw ex
        }
      } else {
        throw new UnsupportedOperationException("GET,HEAD")
      }
    } else {
      if (path == "/json") {
        log.debug("Serving WebSocket connection list (/json endpoint)")
        serveWebSocketConnectionList(connection)
      } else {
        log.warn(s"File request outside base URL ($baseURL): ${connection.uri()}")
        throw new FileNotFoundException()
      }
    }
  }

  private def serveWebSocketConnectionList(connection: Server.Connection): Unit = {
    val json =
      s"""[{
        |  "description": "",
        |  "devtoolsFrontendUrl": "/devtools/inspector.html?ws=$host:$port/dbg",
        |  "webSocketDebuggerUrl": "ws://$host:$port/dbg",
        |  "title": "NCDbg",
        |  "type": "page",
        |  "url": "/"
        |}]
      """.stripMargin
    val bytes = json.getBytes(StandardCharsets.UTF_8)
    val headers = Map("Content-Length" -> bytes.length.toString, "Content-Type" -> "application/json; charset=UTF-8")
    connection.sendResponse(200, "OK", headers.asJava)
    if (connection.method() == "GET") {
      val os = connection.outputStream()
      os.write(bytes)
      os.flush()
    }
  }

  private def serveFile(connection: Server.Connection, file: File): Unit = {
    if (isOkToServe(file)) {
      val ct = mimeTypeOf(file)
      val headers = Map("Content-Length" -> file.length().toString, "Content-Type" -> ct)
      connection.sendResponse(200, "OK", headers.asJava)
      if (connection.method() == "GET") {
        var fis: FileInputStream = null
        try {
          fis = new FileInputStream(file)
          val outChannel = Channels.newChannel(connection.outputStream())
          fis.getChannel.transferTo(0, file.length(), outChannel)
        } finally {
          fis.close()
        }
      }
    } else {
      throw new FileNotFoundException(file.getAbsolutePath)
    }
  }
}

object FileServer {
  val mimeTypeByExtension = Map(
    "js" -> "application/javascript",
    "coffee" -> "application/vnd.coffeescript",
    "txt" -> "text/plain"
  )

  def mimeTypeOf(file: File): String = {
    val name = file.getName
    val extIdx = name.lastIndexOf('.')
    val ext = if (extIdx >= 0) name.substring(extIdx + 1) else ""
    mimeTypeByExtension.getOrElse(ext.toLowerCase, "application/octet-stream")
  }
}