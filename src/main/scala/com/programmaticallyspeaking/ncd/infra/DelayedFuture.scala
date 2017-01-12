package com.programmaticallyspeaking.ncd.infra

import java.util.concurrent.{Executors, TimeUnit}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

// TODO: test
object DelayedFuture {
  private def executor = Executors.newSingleThreadScheduledExecutor()

  def apply[R](delay: FiniteDuration)(fun: => R)(implicit executionContext: ExecutionContext): Future[R] = {
    val resultPromise = Promise[R]()
    executor.schedule(new Runnable {
      override def run(): Unit = {
        Future(fun).onComplete {
          case Success(result) => resultPromise.success(result)
          case Failure(t) => resultPromise.failure(t)
        }
      }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    resultPromise.future
  }
}