package com.programmaticallyspeaking.ncd.infra

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.ser.Serializers
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.fasterxml.jackson.module.scala.{DefaultScalaModule, JacksonModule}
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.RemoteObject

import scala.reflect.ClassTag

/**
  * This class exists because Jackson's `OptionModule` cannot write `Some(null)`
  */
private class RemoteObjectSerializer extends StdSerializer[RemoteObject](classOf[RemoteObject]) {
  override def serialize(value: RemoteObject, gen: JsonGenerator, provider: SerializerProvider): Unit = {
    assert(value.productArity == 7, "Expected RemoteObject to have 7 product elements, but it has " + value.productArity)
    gen.writeStartObject()
    // (`type`: String, subtype: String, className: String, description: String, value: Any, unserializableValue: String, objectId: String)
    write(value, value.`type`, "type", gen)
    write(value, value.subtype, "subtype", gen)
    write(value, value.className, "className", gen)
    write(value, value.description, "description", gen)
    write(value, value.value, "value", gen)
    write(value, value.unserializableValue, "unserializableValue", gen)
    write(value, value.objectId, "objectId", gen)
    gen.writeEndObject()
  }

  private def write(remoteObject: RemoteObject, value: Any, field: String, gen: JsonGenerator): Unit = value match {
    case Some(x) => gen.writeObjectField(field, x)
    case None => // omit
    case other => gen.writeObjectField(field, other)
  }
}

private object RemoteObjectSerializerResolver extends Serializers.Base {
  private val REMOTE_OBJECT = classOf[RemoteObject]


  override def findSerializer(config: SerializationConfig, `type`: JavaType, beanDesc: BeanDescription): JsonSerializer[_] = {
    if (!REMOTE_OBJECT.isAssignableFrom(`type`.getRawClass)) null
    else new RemoteObjectSerializer()
  }
}

trait RemoteObjectModule extends JacksonModule {
  this += (_ addSerializers RemoteObjectSerializerResolver)
}

object ObjectMapping {

  private val mapper = new ObjectMapper()
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.setSerializationInclusion(Include.NON_ABSENT)
  mapper.registerModule(new DefaultScalaModule with RemoteObjectModule)

  def fromJson[R <: Any : ClassTag](json: String): R = {
    val clazz = implicitly[ClassTag[R]].runtimeClass
    fromJson(json, clazz).asInstanceOf[R]
  }

  def fromJson[R](json: String, clazz: Class[R]): R =
    mapper.readValue(json, clazz)

  def toJson[A](obj: A) = mapper.writeValueAsString(obj)

  def fromMap[R](map: Map[String, Any], clazz: Class[R]): R =
    mapper.convertValue(map, clazz)
}
