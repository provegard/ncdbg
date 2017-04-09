package com.programmaticallyspeaking.ncd.infra

import java.io.File
import java.nio.charset.Charset
import java.nio.file.Files

import scala.util.Try

trait FileReader {
  def read(file: File, charset: Charset): Try[String]
}


class FileSystemFileReader extends FileReader {
  override def read(file: File, charset: Charset): Try[String] = {
    Try {
      val bytes = Files.readAllBytes(file.toPath)
      new String(bytes, charset)
    }
  }
}