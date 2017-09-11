package com.programmaticallyspeaking.ncd.infra

object PathUtils {

  def insertNameSuffix(filePath: String, suffix: String): String = {
    val extIdx = filePath.lastIndexOf('.')
    val sepIdx = filePath.lastIndexWhere(ch => ch == '/' || ch == '\\')
    if (extIdx > sepIdx && extIdx >= 0) filePath.substring(0, extIdx) + suffix + filePath.substring(extIdx)
    else filePath + suffix
  }
}
