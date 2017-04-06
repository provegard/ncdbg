package com.programmaticallyspeaking.ncd.nashorn

import java.io.{File, FileNotFoundException}
import java.net.URI
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Files

import com.programmaticallyspeaking.ncd.host.Script
import com.programmaticallyspeaking.ncd.infra.{Hasher, ScriptURL}

class ScriptImpl(path: String, scriptData: Array[Byte], val id: String) extends Script {
  import ScriptImpl._

  val url: ScriptURL = ScriptURL.fromPath(path)

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
}

object ScriptImpl {

  private val UTF8 = StandardCharsets.UTF_8

//  def filePathToUrl(path: String): String = {
//    if (path.startsWith("file:/")) return filePathToUrl(path.substring(6))
//    val parts: Seq[String] = path.split("[/\\\\]").filter(_ != "").toList match {
//      case head :: tail if head.length >= 2 && head(1) == ':' =>
//        Seq(head(0).toString) ++ tail
//      case head :: tail => Seq(head) ++ tail
//      case Nil => Seq.empty
//    }
//    "file://" + parts.mkString("/")
//  }

  def fromSource(path: String, source: String, id: String): Script = {
    val bytes = source.getBytes(UTF8)
    new ScriptImpl(path, bytes, id)
  }
}