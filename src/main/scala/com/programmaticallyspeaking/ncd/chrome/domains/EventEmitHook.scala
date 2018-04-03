package com.programmaticallyspeaking.ncd.chrome.domains

import akka.actor.ActorRef

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}

object EventEmitHook {
  case class WaitingPromise(matcher: PartialFunction[Messages.Event, Boolean], promise: Promise[Seq[Messages.Event]])
}

/**
  * A simple domain synchronization tool. It allows one domain to register a listener to events emitted by
  * another domain.
  *
  * @param keepSize the number of events to keep backwards in time
  */
class EventEmitHook(val keepSize: Int = 20) {
  import EventEmitHook._

  private object lock
  private var recentlyEmitted = ListBuffer[Messages.Event]()
  private var promises = Seq.empty[WaitingPromise]

  /**
    * Emits an event - sends it to a destination actor.
    *
    * @param e the event to send
    * @param destination the destination actor
    */
  def emitEvent(e: Messages.Event, destination: ActorRef): Unit = lock.synchronized {
    destination ! e

    addToQueueMaintainingMaxSize(e)
    fulfillAnyWaitingPromise(e)
  }

  private def isMatch(matcher: PartialFunction[Messages.Event, Boolean], e: Messages.Event) =
    matcher.applyOrElse(e, (_: Messages.Event) => false)

  /**
    * Registers a listener and waits for an event to be emitted. If there are events emitted that match already,
    * they are returned immediately in a completed Future.
    *
    * @param matcher a matcher that determines if an event is the sought one
    * @return a Future that will be completed with events
    */
  def awaitEvent(matcher: PartialFunction[Messages.Event, Boolean]): Future[Seq[Messages.Event]] = lock.synchronized {
    val matching = recentlyEmitted.filter(e => isMatch(matcher, e))
    if (matching.nonEmpty) {
      return Future.successful(matching)
    }

    val promise = Promise[Seq[Messages.Event]]()
    promises :+= WaitingPromise(matcher, promise)
    promise.future
  }

  private def fulfillAnyWaitingPromise(e: Messages.Event): Unit = {
    val (matching, rest) = promises.partition(wp => isMatch(wp.matcher, e))
    promises = rest
    matching.foreach(_.promise.success(Seq(e)))
  }

  private def addToQueueMaintainingMaxSize(e: Messages.Event) = {
    recentlyEmitted += e
    if (recentlyEmitted.size > keepSize) recentlyEmitted = recentlyEmitted.drop(recentlyEmitted.size - keepSize)
  }


}
