package com.programmaticallyspeaking.ncd.messaging

import java.util.concurrent.ConcurrentHashMap

trait Subject[T] extends Observable[T] with Observer[T]

object Subject {
  def serialized[T] = new SerializedSubject[T]
}

/** A combined [[Observable]] and [[Observer]]. Based on RX.
  *
  * RX Design Guidelines §6.4 says to not protect calls to onNext, onError and onComplete, so if any exception is thrown
  * while invoking observers, that exception is re-thrown after all observers have been invoked. If many observers
  * throw, the exceptions are aggregated.
  *
  * @tparam T the item type
  */
class SerializedSubject[T] extends Subject[T] {

  private object lock
  private val observers = new ConcurrentHashMap[String, Observer[T]]()
  private var isDone = false

  override def onNext(item: T): Unit = invokeWithErrorCollection(o => o.onNext(item))

  override def onError(error: Throwable): Unit = invokeWithErrorCollection(beDone(o => o.onError(error)))

  override def onComplete(): Unit = invokeWithErrorCollection(beDone(o => o.onComplete()))

  override def subscribe(observer: Observer[T]): Subscription = {
    val id = java.util.UUID.randomUUID().toString
    observers.put(id, observer)
    () => removeObserverWithId(id)
  }

  private def removeObserverWithId(id: String): Unit = observers.remove(id)

  private def aggregateExceptions(exceptions: List[Exception]): Exception = exceptions match {
    case x :: Nil => x
    case xs =>
      val ex = new RuntimeException("An error occurred during observer dispatching.")
      xs.foreach(ex.addSuppressed)
      ex
  }

  private def beDone(fun: (Observer[T]) => Unit): (Observer[T]) => Unit = { obs =>
    try fun(obs) finally {
      lock.synchronized {
        isDone = true
      }
    }
  }

  private def invokeWithErrorCollection(fun: (Observer[T]) => Unit): Unit = {
    import scala.collection.JavaConverters._
    if (isDone) return
    lock.synchronized {
      var exceptions = List.empty[Exception]
      observers.values().asScala.foreach { observer =>
        try fun(observer) catch {
          case ex: Exception =>
            exceptions :+= ex
        }
      }
      if (exceptions.nonEmpty) throw aggregateExceptions(exceptions)
    }
  }
}