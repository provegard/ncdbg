package com.programmaticallyspeaking.ncd.nashorn

import java.lang.reflect.{InvocationHandler, Method}

import com.programmaticallyspeaking.ncd.boot.{Broker, BrokerConnection}
import org.slf4s.Logging

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

class AttachingHostProxy(broker: Broker, connectionTimeout: FiniteDuration) {

  def createHost(): NashornScriptHost = {
    val clazz = classOf[NashornScriptHost]
    java.lang.reflect.Proxy.newProxyInstance(clazz.getClassLoader, Array(clazz), new Handler).asInstanceOf[NashornScriptHost]
  }

  class Handler extends InvocationHandler with Logging {

    private var brokerConnection: Option[BrokerConnection] = None
    private object connectLock

    override def invoke(proxy: scala.Any, method: Method, args: Array[AnyRef]): AnyRef = {

      brokerConnection match {
        case Some(connection) =>
          val targetHost = connection.host
          val result = method.invoke(targetHost, args: _*)

          if (method.getName == "reset") {
            log.info("Detaching from target debugger")
            connection.disconnect()
            brokerConnection = None
          }

          result

        case None =>
          if (method.getName == "reset") {
            log.warn("Ignoring reset call when there is no connection!")
            return null
          }

          connectFirstThen { connection =>
            val targetHost = connection.host
            method.invoke(targetHost, args: _*)
          }
      }

    }

    private def connectFirstThen(f: BrokerConnection => AnyRef): AnyRef = {
      if (brokerConnection.isEmpty) {
        connectLock.synchronized {
          if (brokerConnection.isEmpty) {
            log.info("Attaching to debug target...")
            val futureConnection = broker.connect({
              _ =>
                // Do we need different handling depending on error or not?
                brokerConnection.foreach(_.disconnect())
                brokerConnection = None
            })
            val connection = Await.result(futureConnection, connectionTimeout)
            brokerConnection = Some(connection)
          }
        }
      }
      f(brokerConnection.get)
    }
  }
}
