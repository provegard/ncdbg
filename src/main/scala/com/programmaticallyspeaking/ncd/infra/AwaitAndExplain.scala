package com.programmaticallyspeaking.ncd.infra

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, TimeoutException}

object AwaitAndExplain {

  def result[R](f: Future[R], timeout: Duration, explain: => String): R = {
    try Await.result(f, timeout) catch {
      case _: TimeoutException => throw new TimeoutException(explain)
    }
  }
}
