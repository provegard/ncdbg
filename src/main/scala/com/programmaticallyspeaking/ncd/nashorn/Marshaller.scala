package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, Undefined}
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.programmaticallyspeaking.ncd.nashorn.mirrors.ScriptObjectMirror
import com.sun.jdi._
import jdk.nashorn.api.scripting.NashornException
import jdk.nashorn.internal.runtime.ScriptObject

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.util.Try

object Marshaller {
  val objectIdGenerator = new IdGenerator("objid-")
  case class ExceptionInfo(stack: String, lineNumber: Int, fileName: String)
}

trait MappingRegistry {
  def register(value: Value, valueNode: ValueNode): Unit
}

class Marshaller(val thread: ThreadReference, mappingRegistry: MappingRegistry) extends ThreadUser {
  import Marshaller._
  import scala.collection.JavaConverters._

  def marshal(value: Value): ValueNode = {
    val result = marshalInPrivate(value)
    mappingRegistry.register(value, result)
    result
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

  private def isIterator(obj: ObjectReference) = inherits(obj, "java.util.Iterator")
  private def isIterable(obj: ObjectReference) = inherits(obj, "java.lang.Iterable")

  private def marshalInPrivate(value: Value): ValueNode = value match {
    case primitive: PrimitiveValue => SimpleValue(marshalPrimitive(primitive))
    case s: StringReference => SimpleValue(s.value())
    case arr: ArrayReference => toArray(arr)
    case so if isScriptObject(so) => marshalScriptObject(so)
    case so if isJSObject(so) => marshalJSObject(so)
    case BoxedValue(vn) => vn
    case UndefinedValue(vn) => vn
    case ExceptionValue(vn) => vn
    case obj: ObjectReference if isIterator(obj) =>
      // Marshal as array - assume we're interested in all values at once
      arrayFromIterator(obj)
    case obj: ObjectReference if isIterable(obj) =>
      // Marshal as array - assume we're interested in all values at once
      arrayFromIterable(obj)
    case obj: ObjectReference =>
      // Unknown, so return something inspectable
      ObjectNode(Map(
        "referenceType" -> LazyNode.eager(SimpleValue(obj.referenceType().name())),
        "uniqueID" -> LazyNode.eager(SimpleValue(obj.uniqueID()))
      ), objectId(obj))
    case x if x == null => EmptyNode
    case other => throw new IllegalArgumentException("Don't know how to marshal: " + other)
  }

  private def isUndefined(value: Value): Boolean = value != null && "jdk.nashorn.internal.runtime.Undefined".equals(value.`type`().name())

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
    val mirror = new ScriptObjectMirror(thread, scriptObject)
    val proxy = new ScriptObjectProxy(mirror, thread, this)
    if (proxy.isArray) toArray(proxy)
    else if (proxy.isFunction) toFunction(proxy)
    else if (proxy.isError) toError(proxy)
    else if (proxy.isDate) toDate(mirror)
    else if (proxy.isRegExp) toRegExp(mirror)
    else {
      //TODO: Date + regexp - but how to marshal to Chrome later?
      // Assume object
      toObject(proxy)
    }
  }

  // Less capable than marshalScriptObject because JSObject doesn't expose as many methods
  private def marshalJSObject(value: Value): ValueNode = {
    val scriptObject = value.asInstanceOf[ObjectReference]
    val mirror = new ScriptObjectMirror(thread, scriptObject)
    val proxy = new ScriptObjectProxy(mirror, thread, this)
    if (proxy.isArray) toArray(proxy)
    else toObject(proxy)
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

  private def arrayFromIterable(obj: ObjectReference) = {
    val invoker = new DynamicInvoker(thread, obj)
    marshal(invoker.iterator())
  }

  private def arrayFromIterator(obj: ObjectReference) = {
    val invoker = new DynamicInvoker(thread, obj)
    val lazyValues = ListBuffer[LazyNode]()
    while (marshalledAs[Boolean](invoker.hasNext())) {
      val nextValue = invoker.next()
      lazyValues += new LazyNode {
        override def resolve(): ValueNode = marshal(nextValue)
      }
    }
    ArrayNode(lazyValues, objectId(obj))
  }

  private def toArray(ref: ArrayReference) = ArrayNode(ref.getValues.asScala.map(marshalLater), objectId(ref))

  private def toArray(proxy: ScriptObjectProxy) = {
    marshal(proxy.mirror.values())
  }

  private def toDate(mirror: ScriptObjectMirror) = {
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString
    // "The toString() method always returns a string representation of the date in American English."
    // The Chrome debugging protocol (in particular, RemoteObject) doesn't seem to care about Date details.
    val stringRep = marshalledAs[String](mirror.actualToString)
    DateNode(stringRep, objectId(mirror.scriptObject))
  }

  private def toRegExp(mirror: ScriptObjectMirror) = {
    val stringRep = marshalledAs[String](mirror.actualToString)
    RegExpNode(stringRep, objectId(mirror.scriptObject))
  }

  private def toObject(proxy: ScriptObjectProxy) = {
    ObjectNode(Map.empty, objectId(proxy.scriptObject))
  }

  private def toFunction(proxy: ScriptObjectProxy) = {
    val invoker = new DynamicInvoker(thread, proxy.scriptObject)
    // getName and toSource are defined in the ScriptFunction class
    val nameValue = invoker.getName()
    val sourceValue = invoker.toSource()

    FunctionNode(getString(marshal(nameValue)), getString(marshal(sourceValue)), objectId(proxy.scriptObject))
  }

  private def toError(proxy: ScriptObjectProxy) = {
    val so = proxy.scriptObject
    def getValue(key: String) = proxy.mirror.get(key)

    val msgValue = getValue("message")
    val nameValue = getValue("name")

    // Nashorn extensions, see https://wiki.openjdk.java.net/display/Nashorn/Nashorn+extensions
    val stackValue = getValue("stack")
    val lineNumberValue = getValue("lineNumber")
    val colNumberValue = getValue("columnNumber")
    val fileNameValue = getValue("fileName")

    val stack = marshalledAsOptionally[String](stackValue).orNull

    val exData = ExceptionData(marshalledAs[String](nameValue),
        marshalledAs[String](msgValue),
        marshalledAsOptionally[Integer](lineNumberValue).getOrElse(Integer.valueOf(0)).intValue(),
        marshalledAsOptionally[Integer](colNumberValue).getOrElse(Integer.valueOf(-1)).intValue(),
        marshalledAsOptionally[String](fileNameValue).getOrElse("<unknown>"), //TODO: To URL?
        Option(stack),
        None
      )
    ErrorValue(exData, isBasedOnThrowable = false, objectId(so))
  }

  private def marshalLater(v: Value) = new LazyMarshalledValue(v)

  private def getString(node: ValueNode) = node match {
    case SimpleValue(k: String) => k
    case other => throw new IllegalArgumentException("Not a string node: " + other)
  }

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
    def unapply(v: Value): Option[ErrorValue] = v match {
      case objRef: ObjectReference =>

        val types = allReachableTypesIncluding(objRef.referenceType())
        val isThrowable = types.exists(_.name() == classOf[Throwable].getName)
        val nashornException = types.find(_.name() == classOf[NashornException].getName)

        if (isThrowable) {
          Some(ErrorValue(exceptionDataOf(objRef, nashornException), isBasedOnThrowable = true, objectId(v)))
        } else None

      case _ => None
    }

    private def extractJavaExceptionInfo(exception: ObjectReference): Option[ExceptionInfo] = {
      val invoker = new DynamicInvoker(thread, exception)
      val stackTraceElements = invoker.getStackTrace().asInstanceOf[ArrayReference]

      if (stackTraceElements.length() > 0) {
        val elemInvoker = new DynamicInvoker(thread, stackTraceElements.getValue(0).asInstanceOf[ObjectReference])
        val lineNumber = marshalledAs[Integer](elemInvoker.getLineNumber())
        val fileName = marshalledAs[String](elemInvoker.getFileName())

        val stack = stackTraceElements.getValues.asScala.map(stackTraceElement => {
          val stInvoker = new DynamicInvoker(thread, stackTraceElement.asInstanceOf[ObjectReference])
          "\tat " + marshalledAs[String](stInvoker.applyDynamic("toString")())
        }).mkString("\n")
        Some(ExceptionInfo(stack, lineNumber, fileName))
      } else None
    }

    private def exceptionDataOf(objRef: ObjectReference, nashornException: Option[ReferenceType]): ExceptionData = {
      val invoker = new DynamicInvoker(thread, objRef)

      // Extract information about the Exception from a Java point of view. This is different than the Nashorn point
      // of view, where only script frames are considered.
      val javaExceptionInfo = extractJavaExceptionInfo(objRef)

      val data: (LocationData, String) = nashornException match {
        case Some(classType: ClassType) =>
          val staticInvoker = new StaticInvoker(thread, classType)
          val stackWithoutMessage = marshalledAs[String](staticInvoker.getScriptStackString(objRef))

          (LocationData(
            marshalledAs[Integer](invoker.getLineNumber()),
            marshalledAs[Integer](invoker.getColumnNumber()),
            marshalledAs[String](invoker.getFileName()) //TODO: new File(_).toURI.toString??
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
      val message = marshalledAs[String](invoker.getMessage())
      val fullStack = s"$name: $message" + Option(data._2).map(st => "\n" + st).getOrElse("")

      // Note the slight implementation inconsistency wrt `fullStack`: we don't prefix with name and message outside
      // of the Option map. The reason is that we always expect Java exception info including stack to be present
      // (we get None if there are no stack frames at all, which would be odd).
      val fullJavaStack = javaExceptionInfo.map(info => s"$name: $message\n${info.stack}")

      ExceptionData(name, message, data._1.lineNumber, data._1.columnNumber, data._1.url, Option(fullStack),
        fullJavaStack)
    }
  }

  /**
    * Convenience function that marshals a value and unpacks the [[SimpleValue]] result to a native type.
    *
    * @param v the value to marshal
    * @tparam R the type of the value to expect and return
    * @return the marshalled and unpacked value
    */
  def marshalledAs[R <: Any : ClassTag](v: Value): R = marshalledAsOptionally(v) match {
    case Some(value) => value
    case None =>
      val runtimeClass = implicitly[ClassTag[R]].runtimeClass
      val vType = if (v == null) "null" else v.getClass.getName
      throw new ClassCastException(s"Cannot extract value of type ${runtimeClass.getName} from $v (of type $vType)")
  }

  /**
    * Convenience function that marshals a value and unpacks the [[SimpleValue]] result to a native type, if possible.
    *
    * @param v the value to marshal
    * @tparam R the type of the value to expect and return
    * @return the marshalled and unpacked value if possible, otherwise `None`
    */
  def marshalledAsOptionally[R <: Any : ClassTag](v: Value): Option[R] = {
    marshal(v) match {
      case SimpleValue(value: R) => Some(value)
      case EmptyNode => Some(null.asInstanceOf[R])
      case other => None
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
