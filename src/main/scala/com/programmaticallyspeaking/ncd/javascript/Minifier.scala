package com.programmaticallyspeaking.ncd.javascript

import com.google.javascript.jscomp.{CompilationLevel, Compiler, CompilerOptions, SourceFile}

object Minifier {

  def minify(code: String): String = {
    val compiler = new Compiler

    val options = new CompilerOptions

    CompilationLevel.SIMPLE_OPTIMIZATIONS.setOptionsForCompilationLevel(options)

    // Don't introduce 'use strict' as it changes the visibility of globals (e.g. Object).
    options.setEmitUseStrict(false)

    // Due to transpilation, we shouldn't need any externs.
    val extern = SourceFile.fromCode("externs.js", "")

    // Errors will be reported for input.js
    val inputFile = SourceFile.fromCode("input.js", code)

    // compile() returns a Result, but it is not needed here.
    compiler.compile(extern, inputFile, options)

    compiler.toSource()
  }
}
