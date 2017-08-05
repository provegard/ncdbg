package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.ClassType

trait TypeLookup {
  def apply(name: String): Option[ClassType]
}
