package com.programmaticallyspeaking.ncd.nashorn

import com.sun.jdi.{Bootstrap, VirtualMachine}
import com.sun.jdi.connect.AttachingConnector

object Connections {

  def connect(host: String, port: Int, timeoutMillis: Int): VirtualMachine =
    connect(getConnector, host, port, timeoutMillis)

  private def getConnector: AttachingConnector = {
    import scala.collection.JavaConverters._
    Bootstrap.virtualMachineManager.attachingConnectors().asScala
      .find(connector => "com.sun.jdi.SocketAttach" == connector.name)
      .getOrElse(throw new IllegalStateException("Failed to find a SocketAttach connector"))
  }

  private def connect(connector: AttachingConnector, host: String, port: Int, timeoutMillis: Int): VirtualMachine = {
    val args = connector.defaultArguments()
    args.get("port").setValue("" + port)
    args.get("hostname").setValue(host)
    args.get("timeout").setValue("" + timeoutMillis)
    connector.attach(args)
  }
}
