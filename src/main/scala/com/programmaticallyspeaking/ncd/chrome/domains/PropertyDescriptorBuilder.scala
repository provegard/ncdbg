package com.programmaticallyspeaking.ncd.chrome.domains

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.PropertyDescriptor
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType}

trait PropertyDescriptorBuilder {
  def from(name: String, desc: ObjectPropertyDescriptor)(implicit remoteObjectConverter: RemoteObjectConverter) = {
    require(name != null && name != "", "Name must be present")
    desc.descriptorType match {
      case PropertyDescriptorType.Generic =>
        PropertyDescriptor(name, desc.isWritable, desc.isConfigurable, desc.isEnumerable, desc.isOwn, None, None, None)

      case PropertyDescriptorType.Data =>
        val remoteValue = desc.value.map(remoteObjectConverter.toRemoteObject(_, byValue = false))
        PropertyDescriptor(name, desc.isWritable, desc.isConfigurable, desc.isEnumerable, desc.isOwn, remoteValue, None, None)

      case PropertyDescriptorType.Accessor =>
        val remoteGetter = desc.getter.map(remoteObjectConverter.toRemoteObject(_, byValue = false))
        val remoteSetter = desc.setter.map(remoteObjectConverter.toRemoteObject(_, byValue = false))
        PropertyDescriptor(name, desc.isWritable, desc.isConfigurable, desc.isEnumerable, desc.isOwn, None,
          remoteGetter, remoteSetter)
    }
  }
}
