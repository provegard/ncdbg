package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{InternalPropertyDescriptor, PropertyDescriptor}
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}

trait InternalPropertyDescriptorBuilder {
  def from(name: String, desc: ObjectPropertyDescriptor)(implicit remoteObjectConverter: RemoteObjectConverter): Option[InternalPropertyDescriptor] = {
    require(name != null && name != "", "Name must be present")
    desc.descriptorType match {
      case PropertyDescriptorType.Data =>
        val remoteValue = desc.value.map(remoteObjectConverter.toRemoteObject)
        Some(InternalPropertyDescriptor(name, remoteValue))

      case _ =>
        None
    }
  }
}
