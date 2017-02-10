package com.programmaticallyspeaking.ncd.chrome.domains

import java.io.File

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ExceptionDetails, RemoteObject}
import com.programmaticallyspeaking.ncd.host.{ErrorValue, ObjectId, ScriptHost, ValueNode}
import org.slf4s.Logging

import scala.util.{Failure, Success}

trait ScriptEvaluateSupport { self: Logging =>

  def evaluate(scriptHost: ScriptHost, callFrameId: String, expression: String, namedObjects: Map[String, ObjectId],
               reportException: Boolean)(implicit remoteObjectConverter: RemoteObjectConverter): EvaluationResult = {
    // TODO: What is the exception ID for?
    val exceptionId = 1

    scriptHost.evaluateOnStackFrame(callFrameId, expression, namedObjects) match {
      case Success(err: ErrorValue) if reportException && err.isBasedOnThrowable =>
        val data = err.data
        // Note that Chrome wants line numbers to be 0-based
        val details = Runtime.ExceptionDetails(exceptionId, data.message, data.lineNumberBase1 - 1, data.columnNumber, Some(data.url))
        // Apparently we need to pass an actual value with the exception details
        EvaluationResult(RemoteObject.undefinedValue, Some(details))
      case Success(err: ErrorValue) if err.isBasedOnThrowable =>
        EvaluationResult(RemoteObject.undefinedValue)
      case Success(result) => EvaluationResult(remoteObjectConverter.toRemoteObject(result))
      case Failure(t) =>

        val exceptionDetails = t.getStackTrace.headOption.flatMap { stackTraceElement =>
          try {
            val lineNumberBase1 = stackTraceElement.getLineNumber
            val url = new File(stackTraceElement.getFileName).toURI.toString
            Some(Runtime.ExceptionDetails(exceptionId, t.getMessage, lineNumberBase1 - 1, 0, Some(url)))
          } catch {
            case e: Exception =>
              log.error(s"Error when trying to construct ExceptionDetails from $stackTraceElement", e)
              None
          }
        }.getOrElse(Runtime.ExceptionDetails(exceptionId, t.getMessage, 0, 0, None))

        EvaluationResult(RemoteObject.undefinedValue, Some(exceptionDetails))
    }

  }

  case class EvaluationResult(result: RemoteObject, exceptionDetails: Option[ExceptionDetails] = None)
}
