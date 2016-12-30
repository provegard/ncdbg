package com.programmaticallyspeaking.ncd.host

trait ObjectRegistry {
  def objectById(id: ObjectId): Option[ComplexNode]
}
