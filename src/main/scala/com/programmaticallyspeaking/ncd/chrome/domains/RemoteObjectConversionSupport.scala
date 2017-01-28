package com.programmaticallyspeaking.ncd.chrome.domains
import com.programmaticallyspeaking.ncd.host.{ScriptHost, ValueNode}

trait RemoteObjectConversionSupport {

  def createRemoteObjectConverter(generatePreview: Boolean)(implicit host: ScriptHost): RemoteObjectConverter = {
    val converter = new RemoteObjectConverterImpl
    if (generatePreview) {
      val options = PreviewGenerator.DefaultOptions
      val generator = new PreviewGenerator(id => host.getObjectProperties(id, onlyOwn = true, onlyAccessors = false), options)
      (value: ValueNode, byValue: Boolean) => {
        val result = converter.toRemoteObject(value, byValue)
        generator.withPreviewForObject(result)
      }
    } else converter
  }
}
