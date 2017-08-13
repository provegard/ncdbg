package com.programmaticallyspeaking.ncd.nashorn

import java.nio.charset.StandardCharsets

import com.programmaticallyspeaking.ncd.host.Script
import com.programmaticallyspeaking.ncd.infra.{Hasher, ScriptURL}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ScriptImpl(path: String, scriptData: Array[Byte], val id: String) extends Script {
  import ScriptImpl._

  val url: ScriptURL = ScriptURL.create(path)

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

  override def statementColumnsForLine(lineNumber1Based: Int): Seq[Int] = //statementCols.getOrElse(lineNumber1Based, Seq.empty)
    lines.lift(lineNumber1Based - 1).map(statementColumnsBase1For).getOrElse(Seq.empty)
}

object ScriptImpl {

  private val UTF8 = StandardCharsets.UTF_8

  // Nashorn supports extended function syntax: function sqrt(x) x * x
  private val funcBodyRegex = "function[^(]*?\\([^)]*\\)[ {]*".r
  private val arrowFuncBodyRegex = "\\([^)]*\\) *=> *[ {]*".r

  def statementColumnsBase1For(sourceLine: String): Seq[Int] = {
    // Leftmost column
    // + all function bodies
    val nonWs = sourceLine.indexWhere(!_.isWhitespace)
    if (nonWs < 0) Seq.empty else
      (Seq(1 + nonWs) ++ Seq(funcBodyRegex, arrowFuncBodyRegex).flatMap(_.findAllMatchIn(sourceLine).map(_.end(0) + 1))).sorted
  }

  def fromSource(path: String, source: String, id: String): Script = {
    val bytes = source.getBytes(UTF8)
    new ScriptImpl(path, bytes, id)
  }
}