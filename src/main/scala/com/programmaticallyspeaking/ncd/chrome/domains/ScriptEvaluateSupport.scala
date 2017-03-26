package com.programmaticallyspeaking.ncd.chrome.domains

import java.io.File

import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ExceptionDetails, RemoteObject}
import com.programmaticallyspeaking.ncd.host.{ErrorValue, ObjectId, ScriptHost}
import com.programmaticallyspeaking.ncd.infra.ObjectMapping
import org.slf4s.Logging

import scala.util.{Failure, Success}

object ScriptEvaluateSupport {
  /**
    * Serialize arguments to JSON so that they can be embedded in a script.
    */
  def serializeArgumentValues(arguments: Seq[Runtime.CallArgument], namedObjects: NamedObjects): Seq[String] = {
    arguments.map { arg =>
      (arg.value, arg.unserializableValue, arg.objectId) match {
        case (Some(value), None, None) =>
          ObjectMapping.toJson(value)
        case (None, Some(unserializableValue), None) => unserializableValue
        case (None, None, Some(strObjectId)) =>
          // Obtain a name for the object with the given ID
          val objectId = ObjectId.fromString(strObjectId)
          namedObjects.useNamedObject(objectId)
        case (None, None, None) => "undefined"
        case _ =>
          // TODO: How can we differ between null and undefined?
          "null"
      }
    }
  }
}

trait ScriptEvaluateSupport { self: Logging =>

  def evaluate(scriptHost: ScriptHost, callFrameId: String, expression: String, namedObjects: Map[String, ObjectId],
               reportException: Boolean)(implicit remoteObjectConverter: RemoteObjectConverter): EvaluationResult = {
    // TODO: What is the exception ID for?
    val exceptionId = 1

    scriptHost.evaluateOnStackFrame(callFrameId, expression, namedObjects) match {
      case Success(err: ErrorValue) if reportException && err.isThrown =>
        val data = err.data
        // Note that Chrome wants line numbers to be 0-based
        val details = Runtime.ExceptionDetails(exceptionId, data.message, data.lineNumberBase1 - 1, data.columnNumber, Some(data.url))
        // Apparently we need to pass an actual value with the exception details
        EvaluationResult(RemoteObject.undefinedValue, Some(details))
      case Success(err: ErrorValue) if err.isThrown =>
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
