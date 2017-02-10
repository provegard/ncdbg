package com.programmaticallyspeaking.ncd.chrome.domains
import com.programmaticallyspeaking.ncd.host.{ScriptHost, ScriptHostBasedObjectInteraction, ValueNode}

trait RemoteObjectConversionSupport {
  //TODO: This needs unit tests!
  def createRemoteObjectConverter(generatePreview: Boolean, byValue: Boolean)(implicit host: ScriptHost): RemoteObjectConverter = {
    val converterToUse = if (byValue) RemoteObjectConverter.byValue(new ScriptHostBasedObjectInteraction(host)) else RemoteObjectConverter.byReference
    if (generatePreview) {
      val options = PreviewGenerator.DefaultOptions
      val generator = new PreviewGenerator(id => host.getObjectProperties(id, onlyOwn = true, onlyAccessors = false), options)
      (value: ValueNode) => {
        val result = converterToUse.toRemoteObject(value)
        generator.withPreviewForObject(result)
      }
    } else converterToUse
  }
}
