package com.programmaticallyspeaking.ncd.nashorn

import java.io.{File, FileNotFoundException}
import java.net.URI
import java.nio.charset.Charset
import java.nio.file.Files

import com.programmaticallyspeaking.ncd.host.Script
import com.programmaticallyspeaking.ncd.infra.Hasher

class ScriptImpl(val uri: String, scriptData: Array[Byte], val id: String) extends Script {
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
}

object ScriptImpl {

  val UTF8 = Charset.forName("utf8")

  def fromFile(path: String, id: String): Script = {
    val file = new File(path)
    // Files.readAllBytes doesn't do this, it seems. Weird!
    if (!file.exists) throw new FileNotFoundException(path)
    new ScriptImpl(file.toURI.toString, Files.readAllBytes(file.toPath), id)
  }

  def fromSource(path: String, source: String, id: String): Script = {
    val bytes = source.getBytes(UTF8)
    new ScriptImpl(new URI(path).toString, bytes, id)
  }
}