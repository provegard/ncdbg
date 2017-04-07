package com.programmaticallyspeaking.ncd.chrome.net

import java.io._
import java.net.{URI, URL}
import java.util.concurrent.ConcurrentHashMap
import javax.activation.{DataSource, FileDataSource}
import javax.annotation.Resource
import javax.xml.ws.{WebServiceContext, _}
import javax.xml.ws.http._

import org.slf4s.Logging

object FileHandler {
  val PathInfoKey = "javax.xml.ws.http.request.pathinfo"
}

@WebServiceProvider
@ServiceMode(value=Service.Mode.MESSAGE)
class FileHandler(fileFilter: (File) => Boolean) extends Provider[DataSource] with Logging {
  import FileHandler._
  @Resource val ctx: WebServiceContext = null

  def invoke(msg: DataSource) = {
    // Is there an exception that allows us to return 400 or 404?? FNF and IllArg result in 500
    contextValue(PathInfoKey) match {
      case Some(path: String) =>
        log.info(s"Request for file with path $path")
        val thePath = if (path.startsWith("/")) path else "/" + path
        val fileURI = new URI("file:" + thePath)
        val file = new File(fileURI)
        if (fileFilter(file)) new FileDataSource(file) else {
          log.warn(s"File at $path is not whitelisted")
          throw new FileNotFoundException(path)
        }

      case _ => throw new IllegalArgumentException("Missing path")
    }
  }

  private def contextValue(key: String): Option[AnyRef] =
    Option(ctx).flatMap(c => Option(c.getMessageContext.get(key)))
}

trait FilePublisher {
  /**
    * Publishes a file and returns the URL under which it has been published.
    *
    * @param file the file to publish
    * @return an URL
    */
  def publish(file: File): URL
}

class FileServer(host: String, port: Int) extends Logging {

  /**
    * The base URL for files, not including a trailing slash.
    */
  val baseURL: String = s"http://$host:$port/files"

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

  def start(): Unit = {
    endpoint = Endpoint.create(HTTPBinding.HTTP_BINDING, new FileHandler(isOkToServe))
    endpoint.publish(baseURL)
  }

  def stop(): Unit = {
    Option(endpoint).foreach(_.stop())
    endpoint = null
  }
}