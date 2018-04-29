package com.programmaticallyspeaking.ncd.infra

import java.lang.reflect.{InvocationHandler, InvocationTargetException, Method}
import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicInteger

import org.slf4s.Logging

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

class ExecutorProxy(executor: Executor) {
  import scala.collection.JavaConverters._

  def createFor[A <: AnyRef : ClassTag](instance: A): A = {
    val clazz = implicitly[ClassTag[A]].runtimeClass
    java.lang.reflect.Proxy.newProxyInstance(clazz.getClassLoader, Array(clazz), new Handler(instance)).asInstanceOf[A]
  }

  class Handler(instance: AnyRef) extends InvocationHandler with Logging {
    import scala.concurrent.ExecutionContext.Implicits._
    private val className = instance.getClass.getName

    private val idGen = new AtomicInteger(0)
    private var awaitingCalls = Map[Int, String]()

    override def invoke(proxy: scala.Any, method: Method, args: Array[AnyRef]): AnyRef = {
      val resultPromise = Promise[AnyRef]()

      val before = System.nanoTime()

      val id = idGen.getAndIncrement()
      val argss = Option(args).getOrElse(Array.empty)
      val desc = s"$method(${argss.mkString(", ")})[$id]"
      log.trace(s"Waiting to execute: $desc")

      // Snapshot of waiting calls prior to submitting to the executor
      val waitingCallsAtEntry = awaitingCalls

      executor.execute(() => {
        log.trace(s"Execute: $id")
        Try(method.invoke(instance, args: _*)) match {
          case Success(f: Future[_]) => resultPromise.completeWith(f.asInstanceOf[Future[AnyRef]])
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
        // Update with this call
        awaitingCalls += (id -> desc)
        //TODO: Configurable timeout
        try Await.result(resultPromise.future, 30.seconds) catch {
          case _: TimeoutException =>
            val other = waitingCallsAtEntry.values
            val sb = new StringBuilder(s"Timed out waiting for '$desc' to complete. Calls at entry: ${other.mkString("'", "', '", "'")}. Stack:\n")
            appendStackTraces(sb)
            log.debug(sb.toString())
            throw new TimeoutException(s"Timed out waiting for '$desc' to complete.")
        } finally {
          // Done with this call
          awaitingCalls -= id
          log.trace(s"Done: $id")
        }
      }
    }

    private def appendStackTraces(sb: StringBuilder): Unit = {
      Thread.getAllStackTraces.asScala.foreach { tup =>
        sb.append("\n> THREAD ").append(tup._1.getName).append("\n")
        tup._2.foreach(ste => sb.append("  ").append(ste).append("\n"))
      }
    }
  }
}
