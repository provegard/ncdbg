package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.host.ObjectId
import com.programmaticallyspeaking.ncd.infra.IdGenerator

import scala.collection.mutable

class NamedObjects {
  private val objectIdNameGenerator = new IdGenerator("__obj_")
  private val namedObjects = mutable.Map[String, ObjectId]()

  def result: Map[String, ObjectId] = namedObjects.toMap

  def useNamedObject(objectId: ObjectId): String = {
    val name = objectIdNameGenerator.next
    namedObjects += name -> objectId
    name
  }

}
