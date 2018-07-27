package com.programmaticallyspeaking.ncd.infra

private case class SourceMapData(sources: Seq[String], sourceRoot: Option[String])

class SourceMap(json: String) {
  private val data = ObjectMapping.fromJson[SourceMapData](json)

  // Transcrypt (Python to JS transpiler) includes an empty entry in the sources section of the source map.
  // Remove it, as it breaks URL resolution.
  // Note: We assume sourceRoot ends with the appropriate slash here.
  def sources: Seq[String] = data.sources.map(resolveAgainstSourceRoot).filter(_ != "")

  private def resolveAgainstSourceRoot(path: String): String = {
    data.sourceRoot.map(_ + path).getOrElse(path)
  }
}

object SourceMap {
  def fromJson(json: String): SourceMap = new SourceMap(json)
}