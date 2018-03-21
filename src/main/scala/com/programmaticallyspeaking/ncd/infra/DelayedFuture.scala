package com.programmaticallyspeaking.ncd.infra

import java.util.concurrent.{Executors, TimeUnit}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{CanAwait, ExecutionContext, Future, Promise}
import scala.util.Try

class CancellableFuture[+A](fut: Future[A], canceller: () => Unit) extends Future[A] {
  override def onComplete[U](f: (Try[A]) => U)(implicit executor: ExecutionContext): Unit = fut.onComplete(f)
  override def isCompleted: Boolean = fut.isCompleted
  override def value: Option[Try[A]] = fut.value
  override def transform[S](f: (Try[A]) => Try[S])(implicit executor: ExecutionContext): Future[S] = fut.transform(f)
  override def transformWith[S](f: (Try[A]) => Future[S])(implicit executor: ExecutionContext): Future[S] = fut.transformWith(f)
  override def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this
  override def result(atMost: Duration)(implicit permit: CanAwait): A = fut.result(atMost)

  def cancel(): Unit = canceller()
}

object DelayedFuture {
  private def executor = Executors.newSingleThreadScheduledExecutor()

  def apply[R](delay: FiniteDuration)(fun: => R)(implicit executionContext: ExecutionContext): CancellableFuture[R] = {
    val resultPromise = Promise[R]()
    var isCancelled = false
    executor.schedule(new Runnable {
      override def run(): Unit = {
        if (!isCancelled)
          resultPromise.completeWith(Future(fun))
      }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    def cancel(): Unit = isCancelled = true
    new CancellableFuture[R](resultPromise.future, () => cancel())
  }
}