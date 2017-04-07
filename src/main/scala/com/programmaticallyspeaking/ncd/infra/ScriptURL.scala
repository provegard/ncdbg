package com.programmaticallyspeaking.ncd.infra

import java.io.File
import java.net.{URI, URL}

final class ScriptURL private[infra](private val uri: URI) {
  import ScriptURL._

  def toFile: File = new File(uri)

  def isFile: Boolean = uri.getScheme == "file"

  override def equals(other: Any): Boolean = other match {
    case that: ScriptURL => uri == that.uri
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(uri)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = uri.toString

  def resolve(pathLike: String): ScriptURL = {
    if (looksLikeRelativePath(pathLike))
      new ScriptURL(uri.resolve(pathLike))
    else
      ScriptURL.fromPath(pathLike)
  }
}

object ScriptURL {
  private[ScriptURL] def looksLikeURL(x: String) =
    x.length > 0 && x(0) != '/' && x.contains(":/")
  private[ScriptURL] def looksLikeRelativePath(x: String) =
    x.length > 0 && x(0) != '/' && !x.lift(1).contains(':')

  def fromURL(url: URL): ScriptURL = new ScriptURL(url.toURI)

  def fromPath(path: String): ScriptURL = {
    val uri = if (looksLikeURL(path)) {
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