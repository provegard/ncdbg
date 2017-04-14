package com.programmaticallyspeaking.ncd.messaging

object Observer {
  /** Creates an observer with only a next handler that invokes the given partal function.
    *
    * @param fun the function to invoke when an item is observed
    * @tparam T the item type
    * @return an observer
    */
  def from[T](fun: PartialFunction[T, Unit]): Observer[T] = new Observer[T] {
    override def onNext(item: T): Unit = fun(item)
    override def onError(error: Throwable): Unit = {}
    override def onComplete(): Unit = {}
  }
}

// Modeled after RX
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
