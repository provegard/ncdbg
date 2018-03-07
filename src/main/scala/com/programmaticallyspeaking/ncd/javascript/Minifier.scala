package com.programmaticallyspeaking.ncd.javascript

import com.google.javascript.jscomp.CompilerOptions.LanguageMode
import com.google.javascript.jscomp.{CompilationLevel, Compiler, CompilerOptions, PropertyRenamingPolicy, SourceFile, VariableRenamingPolicy}

object Minifier {

  def minify(code: String): String = {
    val compiler = new Compiler

    val options = new CompilerOptions

//    options.setQuoteKeywordProperties(true)
//    options.setSkipNonTranspilationPasses(true)
//    options.setVariableRenaming(VariableRenamingPolicy.OFF)
//    options.setPropertyRenaming(PropertyRenamingPolicy.OFF)
//    options.setWrapGoogModulesForWhitespaceOnly(false)
//    options.setPrettyPrint(false)
//    options.setSourceMapOutputPath("/dev/null")
//    options.setSourceMapIncludeSourcesContent(true)
//    //    options.setWarningLevel(ES5_WARNINGS, CheckLevel.OFF)
//    options.setForceLibraryInjection(Iterable("mini_es6_runtime").asJava)

    CompilationLevel.SIMPLE_OPTIMIZATIONS.setOptionsForCompilationLevel(options)

    // Due to transpilation, we shouldn't need any externs.
    val extern = SourceFile.fromCode("externs.js", "")

//    // Wrap the input so that we can read the expression result.
//    var extendedInput = s"var __ncdbg_retval = ($input);"

    // Errors will be reported for input.js
    val inputFile = SourceFile.fromCode("input.js", code)

    // compile() returns a Result, but it is not needed here.
    compiler.compile(extern, inputFile, options)

    // The surrounding IIFE sets 'this' to an anonymous object, so that polyfills (e.g. Symbol) don't pollute the
    // global object. The 'with' statement allows Symbol to be used as a free variable in the generated code.
//    val src = compiler.toSource
//    val output =
//      s"""
//         |(function() {with(this) {
//         |$src
//         |return __ncdbg_retval;
//         |}}).call({})
//         """.stripMargin
//    output
    compiler.toSource()
  }
}
