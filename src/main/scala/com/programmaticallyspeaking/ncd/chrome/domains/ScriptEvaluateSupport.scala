package com.programmaticallyspeaking.ncd.chrome.domains

import java.io.File

import com.google.common.collect.MultimapBuilder.ListMultimapBuilder
import com.programmaticallyspeaking.ncd.chrome.domains.Runtime.{ExceptionDetails, RemoteObject}
import com.programmaticallyspeaking.ncd.host.{ErrorValue, ObjectId, ScriptHost, ValueNode}
import com.programmaticallyspeaking.ncd.infra.{ErrorUtils, IdGenerator, ObjectMapping}
import com.programmaticallyspeaking.ncd.nashorn.InvocationFailedException
import org.slf4s.Logging

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object ScriptEvaluateSupport {
  /**
    * Assuming `arguments` contains a mix of plain values and objects, constructs a function wrapper that takes only
    * objects based on the object IDs and the invokes the given function with all arguments in correct order.
    *
    * @param functionDecl
    * @param arguments
    * @return
    */
  //TODO: Unit test
  def wrapInFunction(functionDecl: String, arguments: Seq[Runtime.CallArgument]): (String, Seq[ObjectId]) = {
    val jsArgs = ListBuffer[String]()
    val objIds = ListBuffer[ObjectId]()
    val objArgNames = ListBuffer[String]()

    val unpackedWithIndex = arguments.map(unpackArg).zipWithIndex
    unpackedWithIndex.foreach {
      case (either, idx) =>
        either match {
          case Left(s) =>
            jsArgs += s
          case Right(objId) =>
            val name = "__o_" + idx
            objIds += objId
            objArgNames += name
            jsArgs += name
        }
    }

    if (objIds.size == arguments.size) {
      // no need for wrapping
      return (functionDecl, objIds)
    }

    val jsArray = jsArgs.mkString("[", ", ", "]")
    val objArgList = objArgNames.mkString(", ")

    val wrapperFun =
      s"""
         |function ($objArgList) {
         |  var argsInOrder = $jsArray;
         |  var f = ($functionDecl);
         |  return f.apply(this, argsInOrder);
         |}
       """.stripMargin
    (wrapperFun, objIds)
  }

  private def unpackArg(arg: Runtime.CallArgument): Either[String, ObjectId] = {
    (arg.value, arg.unserializableValue, arg.objectId) match {
      case (Some(value), None, None) =>
        Left(ObjectMapping.toJson(value))
      case (None, Some(unserializableValue), None) =>
        Left(unserializableValue)
      case (None, None, Some(strObjectId)) =>
        val objectId = ObjectId.fromString(strObjectId)
        Right(objectId)
      case (None, None, None) =>
        Left("void 0") // undefined
      case _ =>
        // TODO: How can we differ between null and undefined?
        Left("null")
    }
  }
}

trait ScriptEvaluateSupport { self: Logging =>

  def callFunctionOn(scriptHost: ScriptHost, callFrameId: String, thisObject: Option[ObjectId], functionDeclaration: String, arguments: Seq[ObjectId])
                    (implicit remoteObjectConverter: RemoteObjectConverter): EvaluationResult = {

    toEvaluationResult(scriptHost.callFunctionOn(callFrameId, thisObject, functionDeclaration, arguments))
  }

  def evaluate(scriptHost: ScriptHost, callFrameId: String, expression: String)
              (implicit remoteObjectConverter: RemoteObjectConverter): EvaluationResult = {

    toEvaluationResult(scriptHost.evaluateOnStackFrame(callFrameId, expression))
  }

  private def toEvaluationResult(t: Try[ValueNode])(implicit remoteObjectConverter: RemoteObjectConverter): EvaluationResult = {
    // TODO: What is the exception ID for?
    val exceptionId = 1
    t match {
      case Success(err: ErrorValue) if err.isThrown =>
        val details = Runtime.ExceptionDetails.fromErrorValue(err, exceptionId)
        log.debug(s"Responding with evaluation error: $details" + err.javaStack.map("\r\n" + _))
        // Apparently we need to pass an actual value with the exception details
        EvaluationResult(RemoteObject.undefinedValue, Some(details))
      case Success(result) => EvaluationResult(remoteObjectConverter.toRemoteObject(result))
      case Failure(t) =>
        val exceptionDetails = exceptionDetailsFromError(t, exceptionId)
        EvaluationResult(RemoteObject.undefinedValue, Some(exceptionDetails))
    }
  }

  protected def exceptionDetailsFromError(t: Throwable, exceptionId: Int): ExceptionDetails = {
    val exception = exceptionFromMessage(t.getMessage)
    // text=Uncaught to mimic Chrome
    val details = Runtime.ExceptionDetails(exceptionId, "Uncaught", 0, 0, None, exception = Some(exception))

    t.getStackTrace.headOption.flatMap { stackTraceElement =>
      try {
        val lineNumberBase1 = stackTraceElement.getLineNumber
        val url = new File(stackTraceElement.getFileName).toURI.toString

        Some(details.copy(lineNumber = lineNumberBase1 - 1, url = Some(url)))
      } catch {
        case e: Exception =>
          log.error(s"Error when trying to construct ExceptionDetails from $stackTraceElement", e)
          None
      }
    }.getOrElse(details)
  }

  private def exceptionFromMessage(msg: String): RemoteObject = {
    val typeAndMessage = ErrorUtils.parseMessage(msg)
    errorObject(typeAndMessage.typ, typeAndMessage.message)
  }

  private val errorObjectIdGen = new IdGenerator("_errobj")
  protected def errorObject(name: String, msg: String): RemoteObject =
    RemoteObject.forError(name, msg, Some(s"$name: $msg"), errorObjectIdGen.next)

  case class EvaluationResult(result: RemoteObject, exceptionDetails: Option[ExceptionDetails] = None)
}
