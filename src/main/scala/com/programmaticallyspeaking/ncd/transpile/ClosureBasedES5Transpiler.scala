package com.programmaticallyspeaking.ncd.transpile

import com.google.javascript.jscomp.CompilerOptions.LanguageMode
import com.google.javascript.jscomp.{CompilationLevel, Compiler, CompilerOptions, PropertyRenamingPolicy, SourceFile, VariableRenamingPolicy}

/**
  * ES5 transpiler based on Google Closure compiler. This transpiler requires that the input to transpile is
  * an expression that evaluates to a value; this is necessary in order to avoid global pollution with runtime
  * polyfills.
  */
class ClosureBasedES5Transpiler extends ES5Transpiler {

  override def transpile(input: String): String = {
    import scala.collection.JavaConverters._
    val compiler = new Compiler

    val options = new CompilerOptions

    // Options copied from BaseTranspiler.setOptions. It would be nice and simple to use BaseTranspiler, but
    // its constructor isn't public and TranspilerBuilder hard-codes the runtime but we want a tiny custom runtime.
    options.setLanguageOut(LanguageMode.ECMASCRIPT5)
    options.setQuoteKeywordProperties(true)
    options.setSkipNonTranspilationPasses(true)
    options.setVariableRenaming(VariableRenamingPolicy.OFF)
    options.setPropertyRenaming(PropertyRenamingPolicy.OFF)
    options.setWrapGoogModulesForWhitespaceOnly(false)
    options.setPrettyPrint(false)
    options.setSourceMapOutputPath("/dev/null")
    options.setSourceMapIncludeSourcesContent(true)
    //    options.setWarningLevel(ES5_WARNINGS, CheckLevel.OFF)
    options.setForceLibraryInjection(Iterable("mini_es6_runtime").asJava)

    CompilationLevel.SIMPLE_OPTIMIZATIONS.setOptionsForCompilationLevel(options)

    // Due to transpilation, we shouldn't need any externs.
    val extern = SourceFile.fromCode("externs.js", "")

    // Wrap the input so that we can read the expression result.
    var extendedInput = s"var __ncdbg_retval = ($input);"

    // Errors will be reported for input.js
    val inputFile = SourceFile.fromCode("input.js", extendedInput)

    // compile() returns a Result, but it is not needed here.
    compiler.compile(extern, inputFile, options)

    // The surrounding IIFE sets 'this' to an anonymous object, so that polyfills (e.g. Symbol) don't pollute the
    // global object. The 'with' statement allows Symbol to be used as a free variable in the generated code.
    val src = compiler.toSource
    val output =
      s"""
         |(function() {with(this) {
         |$src
         |return __ncdbg_retval;
         |}}).call({})
         """.stripMargin
    output
  }
}
