package com.programmaticallyspeaking.ncd.config

import org.rogach.scallop.{ScallopConf, ValueConverter}

case class Address(host: String, port: Int) {
  override def toString = host + ":" + port
}
class AddressConverter extends ValueConverter[Address] {

  val valueRegexp = "([^:]+(?::))?([0-9]+)".r

  override def parse(s: List[(String, List[String])]): Either[String, Option[Address]] = {
    s match {
      case (_, valueRegexp(host, port) :: Nil) :: Nil =>
        // I tried getting rid of the trailing : using a non-capturing group, but it didn't work.
        val theHost = Option(host).map(h => h.dropRight(1)).getOrElse("localhost")
        Right(Some(Address(theHost, port.toInt)))
      case Nil =>
        Right(None)
      case _ =>
        Left("address must have format <host>:<port> or only <port>")
    }
  }

  override val tag = scala.reflect.runtime.universe.typeTag[Address]
  override val argType = org.rogach.scallop.ArgType.SINGLE
}

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  implicit val addressConverter = new AddressConverter

  private val defaultListen = Address("localhost", 7778)
  private val defaultConnect = Address("localhost", 7777)

  banner(
    """Usage: ncdbg [OPTION]...
      |
      |Ncdbg (Nashorn-Chrome-debugger) connects to a debuggable Java process running Nashorn scripts,
      |while acting as a server for Chrome Developer Tools. This makes it possible to debug Nashorn scripts
      |using Chrome.
      |
      |Options:
    """.stripMargin)

  val listen = opt[Address](default = Some(defaultListen),
    descr = s"address to listen on, on <host>:<port> format or port only. Defaults to $defaultListen.")
  val connect = opt[Address](default = Some(defaultConnect),
    descr = s"address to connect to, on <host>:<port> format or port only. Defaults to $defaultConnect.")
  val isLazy = toggle(name = "lazy", default = Some(false))
  verify()
}