package com.programmaticallyspeaking.ncd.infra

private case class SourceMapData(sources: Seq[String])

class SourceMap(json: String) {
  private val data = ObjectMapping.fromJson[SourceMapData](json)

  // Transcrypt (Python to JS transpiler) includes an empty entry in the sources section of the source map.
  // Remove it, as it breaks URL resolution.
  def sources: Seq[String] = data.sources.filter(_ != "")
}

object SourceMap {
  def fromJson(json: String): SourceMap = new SourceMap(json)
}