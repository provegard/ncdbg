package com.programmaticallyspeaking.ncd.infra

private case class SourceMapData(sources: Seq[String])

class SourceMap(json: String) {
  private val data = ObjectMapping.fromJson[SourceMapData](json)

  def sources: Seq[String] = data.sources
}

object SourceMap {
  def fromJson(json: String): SourceMap = new SourceMap(json)
}