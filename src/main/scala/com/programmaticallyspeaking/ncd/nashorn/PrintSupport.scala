package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host.PrintMessage
import com.programmaticallyspeaking.ncd.messaging.Observer
import com.programmaticallyspeaking.ncd.nashorn.NashornDebuggerHost.{InternalState, Pause, Unpause}
import com.sun.jdi.event.{BreakpointEvent, Event}
import com.sun.jdi.request.BreakpointRequest
import com.sun.jdi.{ArrayReference, ClassType, ThreadReference, Value}
import org.slf4s.Logging

object PrintSupport {
  val JSType_toString_signature = "toString(Ljava/lang/Object;)Ljava/lang/String;"
  val Object_toString_signature = "toString()Ljava/lang/String;"
}

/**
  * Responsible for capturing Nashorns 'print' and 'println' extensions, and converting them into [[PrintMessage]]
  * events.
  */
trait PrintSupport { self: NashornDebuggerHost with Logging =>

  import PrintSupport._
  import TypeConstants._
  import com.programmaticallyspeaking.ncd.nashorn.mirrors.Mirrors._
  import JDIExtensions._

  import scala.collection.JavaConverters._

  def enablePrintCapture(global: ClassType): Unit = {
    Breakpoints.enableBreakingAtGlobalPrint(global) { breakpointRequests =>
      breakpointRequests.foreach(_.onEventDo(eventHandler))

      internalStateSubject.subscribe(Observer.from[InternalState] {
        case Pause   => breakpointRequests.foreach(_.disable())
        case Unpause => breakpointRequests.foreach(_.enable())
      })

    }
  }

  private def eventHandler(ev: Event): Boolean = {
    ev match {
      case b: BreakpointEvent =>
        implicit val thread: ThreadReference = b.thread()
        implicit val marshaller: Marshaller = new Marshaller(MappingRegistry.noop)

        // Try to use JSType.toString()
        val jsType = typeLookup(NIR_JSType)
        val jsTypeInvoker = jsType.map(Invokers.shared.getStatic)
        def valueToString(v: Value): String = {
          //TODO: What's a good fallback here?
          jsTypeInvoker.map(_.applyDynamic(JSType_toString_signature)(v).asString).getOrElse("???")
        }

        // Signature is:
        // public static Object print(final Object self, final Object... objects) {
        // So grab the arguments and skip the first. What we have then should be an array!
        val argStrings = b.location().method().arguments().asScala.tail.headOption.map(thread.frame(0).getValue) match {
          case Some(arr: ArrayReference) =>
            arr.getValues.asScala.map(valueToString)
          case _ => Seq.empty
        }

        val msg = argStrings.mkString(" ")
        log.debug(s"Intercepted call to 'print' with message '$msg'")
        emitEvent(PrintMessage(msg))

      case other =>
        log.warn("Unexpected event: " + other)
    }
    // Consume the event!
    true
  }

}
