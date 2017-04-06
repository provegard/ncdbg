package com.programmaticallyspeaking.ncd.infra

import java.io.File
import java.net.URI

final class ScriptURL private[infra](private val uri: URI) {

  def toFile: File = new File(uri)

  override def equals(other: Any): Boolean = other match {
    case that: ScriptURL => uri == that.uri
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(uri)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = uri.toString
}

object ScriptURL {
  def fromPath(path: String): ScriptURL = {
    val uri = if (path.contains(":/")) {
      // Assume this is something resembling an URL already, e.g. file:/foo/bar,
      // but we don't know how many slashes there are.
      var (scheme, rest) = path.span(_ != ':')
      rest = rest.substring(1) // skip the leading :
      val slashCount = rest.prefixLength(_ == '/')
      new URI(scheme, "", "/" + rest.substring(slashCount), null)
    } else {
      val withUnixSlashes = path.replace("\\", "/")
      val uriPart = if (withUnixSlashes.startsWith("/")) withUnixSlashes else "/" + withUnixSlashes
      new URI("file", "", uriPart, null)
    }
    new ScriptURL(uri)
  }
}