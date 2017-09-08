package com.programmaticallyspeaking.ncd.infra

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method}
import java.util.concurrent.{ConcurrentHashMap, Executor}

import org.slf4s.Logging

import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.reflect.ClassTag
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ExecutorProxy(executor: Executor) {

  def createFor[A <: AnyRef : ClassTag](instance: A): A = {
    val clazz = implicitly[ClassTag[A]].runtimeClass
    java.lang.reflect.Proxy.newProxyInstance(clazz.getClassLoader, Array(clazz), new Handler(instance)).asInstanceOf[A]
  }

  class Handler(instance: AnyRef) extends InvocationHandler with Logging {
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.collection.JavaConverters._
    private val className = instance.getClass.getName

    private val awaitingCalls = java.util.Collections.newSetFromMap(new ConcurrentHashMap[Method, java.lang.Boolean]()).asScala

    override def invoke(proxy: scala.Any, method: Method, args: Array[AnyRef]): AnyRef = {
      val resultPromise = Promise[AnyRef]()

      val before = System.nanoTime()

      executor.execute(() => {
        Try(method.invoke(instance, args: _*)) match {
          case Success(f: Future[AnyRef]) => resultPromise.completeWith(f)
          case Success(result) => resultPromise.success(result)
          case Failure(t: InvocationTargetException) => resultPromise.failure(t.getCause)
          case Failure(t) => resultPromise.failure(t)
        }
      })

      resultPromise.future.onComplete { _ =>
        val methodName = method.getName
        val millis = (System.nanoTime() - before).nanos.toMillis
        log.trace(s"Elapsed time for $className.$methodName = $millis ms")
      }

      if (classOf[Future[_]].isAssignableFrom(method.getReturnType)) resultPromise.future
      else {
        awaitingCalls += method
        //TODO: Configurable timeout
        try Await.result(resultPromise.future, 30.seconds) catch {
          case _: TimeoutException =>
            val other = awaitingCalls - method
            throw new TimeoutException(s"Timed out waiting for $method to complete. Outstanding calls: ${other.mkString(", ")}")
        } finally {
          awaitingCalls -= method
        }
      }
    }
  }
}
