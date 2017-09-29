package com.programmaticallyspeaking.ncd.nashorn

import java.nio.charset.StandardCharsets

import com.programmaticallyspeaking.ncd.host.Script
import com.programmaticallyspeaking.ncd.infra.{Hasher, ScriptURL}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ScriptImpl(val url: ScriptURL, scriptData: Array[Byte], val id: String) extends Script {
  import ScriptImpl._

  val contents = new String(scriptData, UTF8)

  val lines: Seq[String] = contents.split("\r?\n")

  val lineCount = lines.length
  val lastLineLength = lines.lastOption.map(_.length).getOrElse(0)

  private var cachedHash: String = _
  private object hashLock
  override def contentsHash(): String = {
    if (cachedHash == null) {
      hashLock.synchronized {
        if (cachedHash == null) {
          cachedHash = Hasher.md5(scriptData)
        }
      }
    }
    cachedHash
  }

  private val sourceMapUrlBegin = "//# sourceMappingURL="
  private val sourceUrlBegin = "//# sourceURL="

  private var _sourceMapUrl: Option[ScriptURL] = None
  private var _sourceUrl: Option[ScriptURL] = None

  lines.foreach { line =>
    if (line.startsWith(sourceMapUrlBegin)) {
      _sourceMapUrl = Some(line.substring(sourceMapUrlBegin.length)).map(url.resolve)
    } else if (line.startsWith(sourceUrlBegin)) {
      _sourceUrl = Some(line.substring(sourceUrlBegin.length)).map(url.resolve)
    }
  }

  override def sourceMapUrl(): Option[ScriptURL] = _sourceMapUrl
  override def sourceUrl(): Option[ScriptURL] = _sourceUrl

  override def sourceLine(lineNumber1Based: Int): Option[String] = {
    lines.lift(lineNumber1Based - 1)
  }

  override def toString: String = url.toString
}

object ScriptImpl {

  private val UTF8 = StandardCharsets.UTF_8

  def fromSource(url: ScriptURL, source: String, id: String): Script = {
    val bytes = source.getBytes(UTF8)
    new ScriptImpl(url, bytes, id)
  }
}