package com.programmaticallyspeaking.ncd.infra

import java.util.Properties

object BuildProperties {

  private val buildPropsStream = getClass.getResourceAsStream("/build.properties")
  private val props = new Properties()
  if (buildPropsStream != null) {
    try props.load(buildPropsStream) finally buildPropsStream.close()
  }

  def version: String = Option(props.getProperty("version")).getOrElse("(unknown)")
}
