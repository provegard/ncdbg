package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, Undefined}
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.programmaticallyspeaking.ncd.nashorn.mirrors._
import com.sun.jdi._
import jdk.nashorn.api.scripting.NashornException
import jdk.nashorn.internal.runtime.ScriptObject

import scala.language.implicitConversions
import scala.util.Try

object Marshaller {
  val objectIdGenerator = new IdGenerator("objid-")
  case class ExceptionInfo(stack: String, lineNumber: Int, fileName: String)

  private[Marshaller] case class ExceptionDataWithJavaStack(data: ExceptionData, javaStack: Option[String])

  private[Marshaller] case class MarshallerResult(valueNode: ValueNode, extraProperties: Map[String, ValueNode])

  implicit def valueNode2MarshallerResult(valueNode: ValueNode): MarshallerResult = MarshallerResult(valueNode, Map.empty)

  def isUndefined(value: Value): Boolean = value != null && "jdk.nashorn.internal.runtime.Undefined".equals(value.`type`().name())
}

object MappingRegistry {
  /**
    * A [[MappingRegistry]] that does not actually do any registration.
    */
  val noop = new MappingRegistry {
    override def register(value: Value, valueNode: ComplexNode, extraProperties: Map[String, ValueNode]): Unit = {}
  }
}

trait MappingRegistry {
  def register(value: Value, valueNode: ComplexNode, extraProperties: Map[String, ValueNode]): Unit
}

class Marshaller(val thread: ThreadReference, mappingRegistry: MappingRegistry) extends ThreadUser {
  import Marshaller._

  import scala.collection.JavaConverters._

  private implicit val marshaller: Marshaller = this

  def marshal(value: Value): ValueNode = {
    val result = marshalInPrivate(value)
    result.valueNode match {
      case c: ComplexNode =>
        mappingRegistry.register(value, c, result.extraProperties)
      case _ =>
    }
    result.valueNode
  }

  private def register(value: Value, c: ComplexNode, extraData: Map[String, ValueNode] = Map.empty): ComplexNode = {
    mappingRegistry.register(value, c, extraData)
    c
  }

  /**
    * Generate an ID for an object. If possible, we generate an ID based on the unique ID of the object reference,
    * so that two references to the same object get the same ID.
    *
    * @param v the value for which an object ID should be generated
    */
  private def objectId(v: Value) = {
    val id = v match {
      case o: ObjectReference => "uid-" + o.uniqueID()
      case _ => objectIdGenerator.next
    }
    ObjectId(id)
  }

  private def marshalInPrivate(value: Value): MarshallerResult = value match {
    case primitive: PrimitiveValue => SimpleValue(marshalPrimitive(primitive))
    case s: StringReference => SimpleValue(s.value())
    case arr: ArrayReference => toArray(arr)
    case so if isScriptObject(so) => marshalScriptObject(so)
    case so if isJSObject(so) => marshalJSObject(so)
    case BoxedValue(vn) => vn
    case UndefinedValue(vn) => vn
    case ExceptionValue((vn, maybeJavaStack)) => MarshallerResult(vn, maybeJavaStack.map(st => "javaStack" -> SimpleValue(st)).toMap)
    case obj: ObjectReference =>
      // Unknown, so return something inspectable
      MarshallerResult(ObjectNode(objectId(obj)), Map(
        "referenceType" -> SimpleValue(obj.referenceType().name()),
        "uniqueID" -> SimpleValue(obj.uniqueID())
      ))
    case x if x == null => EmptyNode
    case other => throw new IllegalArgumentException("Don't know how to marshal: " + other)
  }

  private def attemptUnboxing(value: ObjectReference): Option[ValueNode] = {
    val invoker = new DynamicInvoker(thread, value)
    val v = value.referenceType().name() match {
      case "java.lang.Double" => invoker.doubleValue()
      case "java.lang.Boolean" => invoker.booleanValue()
      case "java.lang.Integer" => invoker.intValue()
      case "java.lang.Long" => invoker.longValue()
      case _ => null
    }
    Option(v).map(marshal)
  }

  private def marshalScriptObject(value: Value): ValueNode = {
    val scriptObject = value.asInstanceOf[ObjectReference]
    val mirror = new ScriptObjectMirror(scriptObject)
    if (mirror.isArray) toArray(mirror)
    else mirror.className match {
      case "Function" => toFunction(mirror.asFunction)
      case "Error" => toError(mirror)
      case "Date" => toDate(mirror)
      case "RegExp" => toRegExp(mirror)
      case _ => toObject(mirror)
    }
  }

  // Less capable than marshalScriptObject because JSObject doesn't expose as many methods
  private def marshalJSObject(value: Value): ValueNode = {

    val jsObject = value.asInstanceOf[ObjectReference]
    val mirror = new JSObjectMirror(jsObject)
    if (mirror.isArray) toArray(mirror)
    else toObject(mirror)
  }

  private def marshalPrimitive(value: Value): Any = value match {
    case b: ByteValue => b.byteValue()
    case b: BooleanValue => b.booleanValue()
    case s: ShortValue => s.shortValue()
    case i: IntegerValue => i.intValue()
    case l: LongValue => l.longValue()
    case f: FloatValue => f.floatValue()
    case d: DoubleValue => d.doubleValue()
    case c: CharValue => c.charValue()
    case _ => throw new UnsupportedOperationException("Unhandled primitive value: " + value)
  }

  def isJSObject(value: Value): Boolean = value match {
    case objRef: ObjectReference if inherits(objRef, "jdk.nashorn.api.scripting.JSObject") => true
    case _ => false
  }

  def isScriptObject(value: Value): Boolean = value match {
    case objRef: ObjectReference =>
      val typeName = value.`type`().name()
      if (typeName.startsWith("jdk.nashorn.internal.scripts.JO")) return true
      Try {
        // TODO: Cache this
        val clazz = Class.forName(typeName)
        classOf[ScriptObject].isAssignableFrom(clazz)
      }.getOrElse(false)
    case _ => false
  }

  private def toArray(ref: ArrayReference) = ArrayNode(ref.length(), objectId(ref))

  private def toArray(mirror: ScriptObjectMirror) = {
    val size = mirror.getInt("length", 0)
    ArrayNode(size, objectId(mirror.scriptObject))
  }

  private def toArray(mirror: JSObjectMirror) = {
    val size = mirror.getInt("length", 0)
    ArrayNode(size, objectId(mirror.jsObject)) // TODO: Items, but we will refactor...
  }

  private def toDate(mirror: ScriptObjectMirror) = {
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString
    // "The toString() method always returns a string representation of the date in American English."
    // The Chrome debugging protocol (in particular, RemoteObject) doesn't seem to care about Date details.
    DateNode(mirror.actualToString, objectId(mirror.scriptObject))
  }

  private def toRegExp(mirror: ScriptObjectMirror) = {
    RegExpNode(mirror.actualToString, objectId(mirror.scriptObject))
  }

  private def toObject(mirror: ScriptObjectMirror) = ObjectNode(objectId(mirror.scriptObject))
  private def toObject(mirror: JSObjectMirror) = ObjectNode(objectId(mirror.jsObject))

  private def toFunction(mirror: ScriptFunctionMirror) = {
    val name = mirror.name
    val source = mirror.source

    FunctionNode(name, source, objectId(mirror.scriptObject))
  }

  private def toError(mirror: ScriptObjectMirror) = {
    val msgValue = mirror.getString("message")
    val nameValue = mirror.getString("name")

    // Nashorn extensions, see https://wiki.openjdk.java.net/display/Nashorn/Nashorn+extensions
    val stackValue = mirror.getString("stack")
    val lineNumberValue = mirror.getInt("lineNumber", 0)
    val colNumberValue = mirror.getInt("columnNumber", -1)
    val fileNameValue = mirror.getString("fileName")

    val exData = ExceptionData(nameValue, msgValue, lineNumberValue, colNumberValue,
      Option(fileNameValue).getOrElse("<unknown>"), //TODO: To URL?
      Option(stackValue)
    )
    ErrorValue(exData, isBasedOnThrowable = false, objectId(mirror.scriptObject))
  }

  private def marshalLater(v: Value) = new LazyMarshalledValue(v)

  class LazyMarshalledValue(v: Value) extends LazyNode {
    override def resolve(): ValueNode = marshal(v)
  }

  object BoxedValue {
    def unapply(v: Value): Option[ValueNode] = v match {
      case value: ObjectReference => attemptUnboxing(value)
      case _ => None
    }
  }

  object UndefinedValue {
    def unapply(v: Value): Option[ValueNode] = v match {
      case value if isUndefined(value) => Some(SimpleValue(Undefined))
      case _ => None
    }
  }

  object ExceptionValue {
    def unapply(v: Value): Option[(ErrorValue, Option[String])] = v match {
      case objRef: ObjectReference =>
        val types = allReachableTypesIncluding(objRef.referenceType())
        val isThrowable = types.exists(_.name() == classOf[Throwable].getName)
        val nashornException = types.find(_.name() == classOf[NashornException].getName)

        if (isThrowable) {
          val data = exceptionDataOf(objRef, nashornException)
          Some((ErrorValue(data.data, isBasedOnThrowable = true, objectId(v)), data.javaStack))
        } else None

      case _ => None
    }

    private def extractJavaExceptionInfo(mirror: ThrowableMirror): Option[ExceptionInfo] = {
//      val mirror = new ThrowableMirror(exception)
      val stackTraceElements = mirror.stackTrace

      val stack = stackTraceElements.map("\tat " + _.actualToString).mkString("\n")
      stackTraceElements.headOption.map(ste => ExceptionInfo(stack, ste.lineNumber, ste.fileName))
    }

    private def exceptionDataOf(objRef: ObjectReference, nashornException: Option[ReferenceType]): ExceptionDataWithJavaStack = {
      val mirror = new ThrowableMirror(objRef)

      // Extract information about the Exception from a Java point of view. This is different than the Nashorn point
      // of view, where only script frames are considered.
      val javaExceptionInfo = extractJavaExceptionInfo(mirror)

      val data: (LocationData, String) = nashornException match {
        case Some(classType: ClassType) =>
          // This is a NashornException instance.
          // TODO: Get the actual error name. One possibility is to parse the message.
          val classMirror = new NashornExceptionClassMirror(classType)
          val stackWithoutMessage = classMirror.getScriptStackString(objRef)
          val nashornMirror = new NashornExceptionMirror(objRef)
          (LocationData(
            nashornMirror.lineNumber,
            nashornMirror.columnNumber,
            nashornMirror.fileName //TODO: new File(_).toURI.toString??
          ), stackWithoutMessage)
        case _ =>
          javaExceptionInfo match {
            case Some(info) =>
              // Note that we don't include the Java stack here, because we want the stack trace to be a script stack
              // trace always, for consistency. Instead we include the Java stack trace as an extra Error property.
              (LocationData(info.lineNumber, -1, info.fileName), null)
            case None =>
              (LocationData(0, -1, "<unknown>"), null)
          }
      }

      // Use full Exception type name, e.g. java.lang.IllegalArgumentException
      val name = objRef.referenceType().name()
      val message = mirror.message
      val fullStack = s"$name: $message" + Option(data._2).map(st => "\n" + st).getOrElse("")

      // Note the slight implementation inconsistency wrt `fullStack`: we don't prefix with name and message outside
      // of the Option map. The reason is that we always expect Java exception info including stack to be present
      // (we get None if there are no stack frames at all, which would be odd).
      val fullJavaStack = javaExceptionInfo.map(info => s"$name: $message\n${info.stack}")

      ExceptionDataWithJavaStack(ExceptionData(name, message, data._1.lineNumber, data._1.columnNumber, data._1.url, Option(fullStack)),
        fullJavaStack)
    }
  }

  private def allReachableTypesIncluding(refType: ReferenceType): Seq[ReferenceType] = refType match {
    case ct: ClassType =>
      Seq(ct) ++ allReachableTypesIncluding(ct.superclass()) ++ ct.interfaces().asScala.flatMap(allReachableTypesIncluding)
    case it: InterfaceType =>
      Seq(it) ++ it.superinterfaces().asScala.flatMap(allReachableTypesIncluding)
    case _ => Seq.empty
  }

  /**
    * Tests if the given object inherits a type (class or interface) with the given name.
    * @param obj the object
    * @param typeName the full type name
    */
  private def inherits(obj: ObjectReference, typeName: String) = allReachableTypesIncluding(obj.referenceType()).exists(_.name() == typeName)

  private case class LocationData(lineNumber: Int, columnNumber: Int, url: String)
}
