package com.programmaticallyspeaking.ncd.nashorn

import com.programmaticallyspeaking.ncd.host._
import com.programmaticallyspeaking.ncd.host.types.{ObjectPropertyDescriptor, PropertyDescriptorType, Undefined}
import com.programmaticallyspeaking.ncd.javascript.Minifier
import com.sun.jdi._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait PropertyHolder {
  def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)]
}

class ArrayPropertyHolder(array: ArrayReference)(implicit marshaller: Marshaller) extends PropertyHolder {
  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)] = {
    // Note: A ValueNode shouldn't be null/undefined, so use Some(...) rather than Option(...) for the value
    def createProp(value: ValueNode) =
      ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
        isWritable = true, isOwn = true, Some(value), None, None)

    if (onlyAccessors) Seq.empty
    else {
      // Just return index properties + length.
      (0 until array.length()).map { idx =>
        val theValue = marshaller.marshal(array.getValue(idx))
        idx.toString -> createProp(theValue)
      } :+ ("length" -> createProp(SimpleValue(array.length())))
    }
  }
}

class MapPropertyHolder(values: Map[String, ValueNode]) extends PropertyHolder {
  // Note: A ValueNode shouldn't be null/undefined, so use Some(...) rather than Option(...) for the value
  private def createProp(value: ValueNode) =
    ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
      isWritable = true, isOwn = true, Some(value), None, None)

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)] = {
    if (onlyAccessors) Seq.empty
    else values.map(e => e._1 -> createProp(e._2)).toSeq
  }
}

class ArbitraryObjectPropertyHolder(obj: ObjectReference)(implicit marshaller: Marshaller) extends PropertyHolder {
  import ArbitraryObjectPropertyHolder._

  import scala.collection.JavaConverters._
  private val refType = obj.referenceType()

  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)] = {
    var props = javaBeansMap(onlyOwn)
    if (!onlyAccessors) props = fieldMap(onlyOwn) ++ props
    props
  }

  private def fieldMap(onlyOwn: Boolean): Seq[(String, ObjectPropertyDescriptor)] = {
    val fields = if (onlyOwn) refType.fields() else refType.allFields()
    fields.asScala.map { f =>
      val isOwn = f.declaringType() == refType
      val theValue = marshaller.marshal(obj.getValue(f))
      f.name() -> ObjectPropertyDescriptor(PropertyDescriptorType.Data, isConfigurable = false, isEnumerable = true,
        isWritable = !f.isFinal, isOwn = isOwn, Some(theValue), None, None)
    }
  }

  private def javaBeansMap(onlyOwn: Boolean): Seq[(String, ObjectPropertyDescriptor)] = {
    val methods = if (onlyOwn) refType.methods() else refType.allMethods() //TODO: Test non-all here!
    methods.asScala
      .flatMap(methodToJavaBeanMethod) // filter out JavaBean methods
      .groupBy(_.propertyName)
      .filter(g => fitTogether(g._2))
      .map { g =>
        val getter = g._2.find(_.isGetter).map(_.toFunctionNode)
        val setter = g._2.find(!_.isGetter).map(_.toFunctionNode)
        val isOwn = g._2.head.method.declaringType() == refType
        g._1 -> ObjectPropertyDescriptor(PropertyDescriptorType.Accessor,
          false, true, setter.isDefined, isOwn, None, getter, setter)
      }.toSeq
  }
}

object ArbitraryObjectPropertyHolder {
  import scala.collection.JavaConverters._

  private case class JavaBeansMethod(propertyName: String, method: Method, isGetter: Boolean) {
    private[nashorn] def toFunctionNode: FunctionNode = {
      // As far as I can tell, DevTools doesn't care about a property getter/setter other than what they signify.
      // When a getter is clicked in the UI, the property value is fetched "normally". This is good, because I don't
      // know how to marshal a JDI Method as an ObjectReference via Marshaller.
      val id = Seq(method.declaringType().name(), method.name(), method.signature()).mkString(";")
      val oid = ObjectId(id)
      FunctionNode(method.name(), s"function ${method.name()}() { [native code] }", oid)
    }
  }

  private val nameRegexp = "^[gs]et(\\p{Lu})(.*)".r
  private val VoidTypeName = "void"

  private[nashorn] def extractJavaBeansPropertyName(name: String): Option[String] = name match {
    case nameRegexp(first, rest) => Some(first.toLowerCase + rest)
    case _ => None
  }

  private def methodToJavaBeanMethod(method: Method): Option[JavaBeansMethod] = {
    extractJavaBeansPropertyName(method.name()).map { propName =>
      JavaBeansMethod(propName, method, method.name().startsWith("get"))
    }.filter(meetsBeanRequirements)
  }

  private def meetsBeanRequirements(m: JavaBeansMethod): Boolean = {
    if (m.isGetter) {
      m.method.returnTypeName() != VoidTypeName && m.method.argumentTypeNames().isEmpty
    } else {
      m.method.returnTypeName() == VoidTypeName && m.method.argumentTypeNames().size() == 1
    }
  }

  // Assumes meetsBeanRequirements is true for both
  private def fitTogether(ms: Seq[JavaBeansMethod]): Boolean = {
    // Getter/setter or only one
    ms.size <= 2 &&
    // isGetter must be different across all
      ms.map(_.isGetter).distinct.size == ms.size &&
    // all must belong to the same class
      ms.map(_.method.declaringType()).distinct.size == 1 &&
    // All non-void types should be the same (essentially this is a return-parameter type match)
      ms.flatMap(m => m.method.argumentTypeNames().asScala :+ m.method.returnTypeName()).filter(_ != VoidTypeName).distinct.size == 1
  }
}

trait Extractor {
  def extract(target: Value, onlyOwn: Boolean, onlyAccessors: Boolean)(implicit threadReference: ThreadReference): Value
}

object ScriptBasedPropertyHolderFactory {
  // Note 1: Java.to doesn't wrap a ScriptObject in a ScriptObjectMirror when the target type is an array type. This is
  // good, since we don't want the __proto__ value to be mirrored, since that has negative consequences:
  // - a new mirror is created each time, which breaks the object properties cache
  // - the object properties proto test end up in infinite recursion (this one can be fixed though...)

  // Note 2: The property blacklist RegExp is currently only used for a native ScriptObject.

  // Note 3: Nashorn in Java 9 (build 9+181) doesn't support Object.getOwnPropertyDescriptor with a Symbol,
  // which Node does, so currently we get the property value by accessing the symbol property of the object.
  private val extractorFunctionSource =
    """(function () {
      |  var hasJavaTo = typeof Java !== "undefined" && typeof Java.to === "function";
      |  var hasSymbols = !!Object.getOwnPropertySymbols;
      |  return function __getprops(target, isNative, onlyOwn, onlyAccessors, strPropertyBlacklistRegExp) {
      |    var result = [], proto, i, j;
      |    if (isNative) {
      |      var blacklistRegExp = strPropertyBlacklistRegExp ? new RegExp(strPropertyBlacklistRegExp) : null;
      |      var includeProp = function (prop) { return (blacklistRegExp ? !prop.match(blacklistRegExp) : true) };
      |      var current = target, own = true;
      |      while (current) {
      |        var names = Object.getOwnPropertyNames(current);
      |        for (i = 0, j = names.length; i < j; i++) {
      |          var k = names[i];
      |          if (!includeProp(k)) continue;
      |          var desc = Object.getOwnPropertyDescriptor(current, k);
      |          if (onlyAccessors && !desc.get && !desc.set) continue;
      |          var f_c = desc.configurable ? "c" : "";
      |          var f_e = desc.enumerable ? "e" : "";
      |          var f_w = desc.writable ? "w" : "";
      |          var f_o = own ? "o" : "";
      |          result.push(k,
      |                      (f_c + f_e + f_w + f_o).toString(), // ConsString -> String when Java.to not available
      |                      desc.value,
      |                      desc.get,
      |                      desc.set,
      |                      null); // symbol
      |        }
      |        var symbols = hasSymbols && !onlyAccessors ? Object.getOwnPropertySymbols(current) : [];
      |        for (i = 0, j = symbols.length; i < j; i++) {
      |          var sym = symbols[i];
      |          result.push(sym.toString(),
      |                      own ? "o" : "",
      |                      current[sym], // see Note 3 above
      |                      null,
      |                      null,
      |                      sym);
      |        }
      |        if (own && !onlyAccessors && includeProp("__proto__") && (proto = safeGetProto(current))) {
      |          result.push("__proto__",
      |                      "wo",  // writable + own (not sure about configurable and enumerable)
      |                      proto,
      |                      null,
      |                      null,
      |                      null); // symbol
      |        }
      |        if (own && onlyOwn) current = null; else {
      |          current = safeGetProto(current);
      |          own = false;
      |        }
      |      }
      |    } else if (!onlyAccessors) {
      |      if (Array.isArray(target)) {
      |        for (var i = 0; i < target.length; i++) {
      |          result.push(i.toString(),
      |                      "wo", // writable (correct?) + own
      |                      target[i],
      |                      null,
      |                      null,
      |                      null); // symbol
      |        }
      |        result.push("length",
      |                    "o",
      |                    target.length,
      |                    null,
      |                    null,
      |                    null); // symbol
      |      } else {
      |        for (var k in target) {
      |          result.push(k.toString(),
      |                      "wo", // writable (correct?) + own
      |                      target[k],
      |                      null,
      |                      null,
      |                      null); // symbol
      |        }
      |      }
      |    }
      |    return hasJavaTo ? Java.to(result, "java.lang.Object[]") : result;
      |  };
      |  function safeGetProto(x) {
      |    try {
      |      return x.__proto__;
      |    } catch (e) {
      |      return null;
      |    }
      |  }
      |})();
    """.stripMargin
  private val extractorFunctionSourceMinified = Minifier.minify(extractorFunctionSource)
}

class ScriptBasedPropertyHolderFactory(codeEval: (String) => Value, executor: (Value, Seq[Any], ThreadReference) => Value) {

  private val extractorFunction = codeEval(ScriptBasedPropertyHolderFactory.extractorFunctionSourceMinified)

  def create(obj: ObjectReference, propertyBlacklistRegExp: String, isNative: Boolean)(implicit marshaller: Marshaller): PropertyHolder = {
    extractorFunction match {
      case err: ThrownExceptionReference =>
        marshaller.throwError(err)
      case _ =>

        val extractor = new Extractor {
          override def extract(target: Value, onlyOwn: Boolean, onlyAccessors: Boolean)(implicit threadReference: ThreadReference): Value = {
            // Pass strings to avoid the need for boxing
            executor(extractorFunction,
              Seq(target, asString(isNative), asString(onlyOwn), asString(onlyAccessors), propertyBlacklistRegExp),
              threadReference)
          }
        }

        new ScriptBasedPropertyHolder(obj, extractor)
    }
  }

  // Converts the Boolean to a string that evaluates to true or false in JS.
  private def asString(b: Boolean) = if (b) "true" else ""
}

class ScriptBasedPropertyHolder(obj: ObjectReference, extractor: Extractor)(implicit marshaller: Marshaller) extends PropertyHolder {
  import scala.collection.JavaConverters._

  private def toOption(vn: ValueNode) = vn match {
    case EmptyNode | SimpleValue(Undefined) => None
    case other => Some(other)
  }
  private def populateFromArray(arr: ArrayReference, list: ListBuffer[(String, ObjectPropertyDescriptor)]): Unit = {
    val values = arr.getValues.asScala
    values.grouped(6).map(_.toList).foreach {
      case (key: StringReference) :: (flags: StringReference) :: value :: getter :: setter :: symbol :: Nil =>
        val keyStr = key.value()
        val flagsStr = flags.value()
        var vn = toOption(marshaller.marshal(value))
        val gn = toOption(marshaller.marshal(getter))
        val sn = toOption(marshaller.marshal(setter))
        if (vn.isEmpty && gn.isEmpty && sn.isEmpty)
          vn = Some(SimpleValue(Undefined))
        val descType = if (gn.isDefined || sn.isDefined) PropertyDescriptorType.Accessor else PropertyDescriptorType.Data
        val isConfigurable = flagsStr.contains('c')
        val isEnumerable = flagsStr.contains('e')
        val isWritable = flagsStr.contains('w')
        val isOwn = flagsStr.contains('o')
        val sym = Option(symbol).map(marshaller.marshal).collect { case sn: SymbolNode => sn }
        list += keyStr -> ObjectPropertyDescriptor(descType, isConfigurable, isEnumerable, isWritable, isOwn, vn, gn, sn, sym)
      case other =>
        throw new RuntimeException("Unexpected result from the extractor function: " + other)
    }
  }


  override def properties(onlyOwn: Boolean, onlyAccessors: Boolean): Seq[(String, ObjectPropertyDescriptor)] = {
    implicit val thread = marshaller.thread
    val ret = extractor.extract(obj, onlyOwn, onlyAccessors)

    val list = ListBuffer[(String, ObjectPropertyDescriptor)]()
    ret match {
      case arr: ArrayReference => populateFromArray(arr, list)
      case obj: ObjectReference =>
        val inv = Invokers.shared.getDynamic(obj)
        // Call NativeArray.asObjectArray()
        inv.asObjectArray() match {
          case arr: ArrayReference => populateFromArray(arr, list)
          case other =>
            throw new RuntimeException("Not an array from NativeArray.asObjectArray: " + other)
        }
      case err: ThrownExceptionReference =>
        marshaller.throwError(err)
      case other =>
        throw new RuntimeException("Object property extractor returned unknown: " + other)
    }
    list
  }
}