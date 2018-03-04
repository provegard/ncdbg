package com.programmaticallyspeaking.ncd.transpile

trait ES5Transpiler {
  /**
    * Transpiles script input and returns ES5 code. This method is thread safe.
    *
    * @param input the script to transpile
    * @return transpiled ES5 code
    */
  def transpile(input: String): String
}
