package com.programmaticallyspeaking.ncd.chrome.net

import java.io._
import java.net.{URI, URL}
import java.nio.channels.Channels
import java.util.concurrent.ConcurrentHashMap
import javax.activation.FileTypeMap
import javax.xml.ws._

import com.programmaticallyspeaking.tinyws.Server
import org.slf4s.Logging

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
  /**
    * The base URL for files, not including a trailing slash.
    */
  val baseURL: String = s"http://$host:$port/files"

  private val baseURI = new URI(baseURL)
  private val fileTypeMap = FileTypeMap.getDefaultFileTypeMap

  private var endpoint: Endpoint = _
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
    val path = baseURI.relativize(connection.uri()).getPath
    val method = connection.method()
    if (method == "GET" || method == "HEAD") {
      log.info(s"Request ($method) for file with path $path")
      val thePath = if (path.startsWith("/")) path else "/" + path
      val fileURI = new URI("file:" + thePath)
      val file = new File(fileURI)
      try serveFile(connection, file) catch {
        case _: FileNotFoundException =>
          log.warn(s"File at $path is not whitelisted")
          connection.sendResponse(404, "Not Found", null)
      }
    } else {
      connection.sendResponse(405, "Method Not Allowed", Map("Allow" -> "GET,HEAD").asJava)
    }
    connection.outputStream().close()
  }

  private def serveFile(connection: Server.Connection, file: File): Unit = {
    if (isOkToServe(file)) {
      val ct = fileTypeMap.getContentType(file)
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