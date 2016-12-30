package com.programmaticallyspeaking.ncd.messaging

trait Subject[T] extends Observable[T] with Observer[T]

object Subject {
  def serialized[T] = new SerializedSubject[T]
}

// Based on RX
// RX Design Guidelines §6.4 says to not protect calls to onNext, onError and onComplete.
class SerializedSubject[T] extends Subject[T] {

  private object lock
  private var observers = Map[String, Observer[T]]()

  override def onNext(item: T): Unit = invokeWithErrorCollection(o => o.onNext(item))

  override def onError(error: Throwable): Unit = invokeWithErrorCollection(o => o.onError(error))

  override def onComplete(): Unit = invokeWithErrorCollection(o => o.onComplete())

  override def subscribe(observer: Observer[T]): Subscription = lock.synchronized {
    val id = java.util.UUID.randomUUID().toString
    observers += (id -> observer)
    new Subscription {
      override def unsubscribe(): Unit = removeObserverWithId(id)
    }
  }

  private def removeObserverWithId(id: String): Unit = lock.synchronized {
    observers -= id
  }

  private def aggregateExceptions(exceptions: List[Exception]): Exception = {
    val ex = new RuntimeException("An error occurred during observer dispatching.")
    exceptions.foreach(ex.addSuppressed)
    ex
  }

  private def invokeWithErrorCollection(fun: (Observer[T]) => Unit): Unit = lock.synchronized {
    var exceptions = List.empty[Exception]
    observers.foreach { case (_, observer) =>
      try fun(observer) catch {
        case ex: Exception =>
          exceptions :+= ex
      }
    }
    if (exceptions.nonEmpty) throw aggregateExceptions(exceptions)
  }
}