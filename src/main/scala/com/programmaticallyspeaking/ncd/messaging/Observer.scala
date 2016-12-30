package com.programmaticallyspeaking.ncd.messaging

// Modeled after TX
trait Observer[T] {
  def onNext(item: T): Unit

  def onError(error: Throwable): Unit

  def onComplete(): Unit
}

trait Subscription {
  def unsubscribe(): Unit
}

trait Observable[T] {

  def subscribe(observer: Observer[T]): Subscription
}
