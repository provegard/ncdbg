package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ExceptionData, Undefined}
import com.programmaticallyspeaking.ncd.infra.IdGenerator
import com.sun.jdi._
import jdk.nashorn.api.scripting.NashornException
import jdk.nashorn.internal.runtime.ScriptObject

import scala.reflect.ClassTag
import scala.util.Try

object Marshaller {
  val objectIdGenerator = new IdGenerator("objid-")
}

trait MappingRegistry {
  def register(value: Value, valueNode: ValueNode): Unit
}

class Marshaller(val thread: ThreadReference, mappingRegistry: MappingRegistry) extends ThreadUser {
  import Marshaller._
  import scala.collection.JavaConversions._

  def marshal(value: Value): ValueNode = {
    val result = marshalInPrivate(value)
    mappingRegistry.register(value, result)
    result
  }

  private def objectId = ObjectId(objectIdGenerator.next)

  private def marshalInPrivate(value: Value): ValueNode = value match {
    case primitive: PrimitiveValue => SimpleValue(marshalPrimitive(primitive))
    case s: StringReference => SimpleValue(s.value())
    case arr: ArrayReference => toArray(arr)
    case so if isScriptObject(so) => marshalScriptObject(so)
    case BoxedValue(vn) => vn
    case UndefinedValue(vn) => vn
    case ExceptionValue(vn) => vn
    case obj: ObjectReference =>
      // Unknown, so return something inspectable
      ObjectNode(Map(
        "className" -> LazyNode.eager(SimpleValue(value.getClass.getName)),
        "typeName" -> LazyNode.eager(SimpleValue(value.`type`().name()))
      ), objectId)
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
    val proxy = new ScriptObjectProxy(scriptObject, thread, this)
    if (proxy.isArray) toArray(proxy)
    else if (proxy.isFunction) toFunction(proxy)
    else if (proxy.isError) toError(proxy)
    else if (proxy.isDate) toDate(proxy)
    else {
      //TODO: Date + regexp - but how to marshal to Chrome later?
      // Assume object
      toObject(proxy)
    }
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

  private def isScriptObject(value: Value): Boolean = value match {
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

  private def toArray(ref: ArrayReference) = ArrayNode(ref.getValues.map(marshalLater), objectId)

  private def toArray(proxy: ScriptObjectProxy) = {
    val entrySet = proxy.entrySet()
    val items = entrySet.map {
      case (key, lazyValue) =>
        val idx = key match {
          case SimpleValue(idx: String) => idx.toInt // TODO: Catch
          case other => throw new IllegalArgumentException("Unknown index: " + other)
        }
        idx -> lazyValue
    }.toSeq.sortWith((a, b) => a._1 < b._1).map(_._2)
    ArrayNode(items, objectId)
  }

  private def toDate(proxy: ScriptObjectProxy) = {
    val invoker = new DynamicInvoker(thread, proxy.scriptObject)
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString
    // "The toString() method always returns a string representation of the date in American English."
    // The Chrome debugging protocol (in particular, RemoteObject) doesn't seem to care about Date details.
    val stringRep = marshalledAs[String](invoker.applyDynamic("toString")())
    DateNode(stringRep, objectId)
  }

  private def toObject(proxy: ScriptObjectProxy) = {
    val entrySet = proxy.entrySet()
    val data = entrySet.map {
      case (key, lazyValue) => getString(key) -> lazyValue
    }
    ObjectNode(data, objectId)
  }

  private def toFunction(proxy: ScriptObjectProxy) = {
    val data = toObject(proxy).data
    val invoker = new DynamicInvoker(thread, proxy.scriptObject)
    // getName and toSource are defined in the ScriptFunction class
    val nameValue = invoker.getName()
    val sourceValue = invoker.toSource()

    FunctionNode(getString(marshal(nameValue)), getString(marshal(sourceValue)), data, objectId)
  }

  private def toError(proxy: ScriptObjectProxy) = {
    val so = proxy.scriptObject
    val invoker = new DynamicInvoker(thread, so)
    val getSignature = "get(Ljava/lang/Object;)Ljava/lang/Object;"
    // Convenience method to avoid noise since we call a full-signature method (which we do since there are overloads
    // and we want to select the correct method).
    def getValue(key: String) = invoker.applyDynamic(getSignature)(key)

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
        Option(stack)
      )
    ErrorValue(exData, objectId)
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

        val classes = collectClassTypes(objRef.referenceType())
        val isThrowable = classes.exists(_.name() == classOf[Throwable].getName)
        val nashornException = classes.find(_.name() == classOf[NashornException].getName)

        if (isThrowable) {
          Some(ErrorValue(exceptionDataOf(objRef, nashornException), objectId))
        } else None

      case _ => None
    }

    private def exceptionDataOf(objRef: ObjectReference, nashornException: Option[ClassType]): ExceptionData = {
      val invoker = new DynamicInvoker(thread, objRef)

      val data: (LocationData, String) = nashornException match {
        case Some(classType) =>
          val staticInvoker = new StaticInvoker(thread, classType)
          val stackWithoutMessage = marshalledAs[String](staticInvoker.getScriptStackString(objRef))

          (LocationData(
            marshalledAs[Integer](invoker.getLineNumber()),
            marshalledAs[Integer](invoker.getColumnNumber()),
            marshalledAs[String](invoker.getFileName()) //TODO: new File(_).toURI.toString??
            ), stackWithoutMessage)
        case None => (LocationData(0, -1, "<unknown>"), null)
      }

      val name = "Error"
      val message = marshalledAs[String](invoker.invokeParameterLessMethod(objRef, "getMessage")) // SimpleValue(String)
      val fullStack = s"$name: $message" + Option(data._2).map(st => "\n" + st)
//      val cause = marshal(invoker.invokeParameterLessMethod(objRef, "getCause")) // ErrorValue
//      val stack = marshal(invoker.invokeParameterLessMethod(objRef, "getStackTrace")) // ArrayNode
      // NashornException.getScriptStackString
      ExceptionData(name, message, data._1.lineNumber, data._1.columnNumber, data._1.url, Option(fullStack))
    }
  }

  private def marshalledAs[R <: Any : ClassTag](v: Value): R = marshalledAsOptionally(v) match {
    case Some(value) => value
    case None => throw new ClassCastException(s"Cannot extract value from $v")
  }

  private def marshalledAsOptionally[R <: Any : ClassTag](v: Value): Option[R] = {
    val runtimeClass = implicitly[ClassTag[R]].runtimeClass
    marshal(v) match {
      case SimpleValue(value) if runtimeClass.isInstance(value) => Some(value.asInstanceOf[R])
      case other => None
    }
  }

  private def collectClassTypes(refType: ReferenceType): Seq[ClassType] = refType match {
    case ct: ClassType =>
      Seq(ct) ++ collectClassTypes(ct.superclass())
    case _ => Seq.empty
  }

  private case class LocationData(lineNumber: Int, columnNumber: Int, url: String)
}
