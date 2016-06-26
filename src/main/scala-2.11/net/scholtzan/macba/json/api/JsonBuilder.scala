package net.scholtzan.macba.json.api

import java.io.Writer

import net.scholtzan.macba.json.stream.JsonStreamWriter
import org.json4s.JsonAST._


/** The JSON context is intended to signify the enclosing structure the JSON is written into.
  *
  * These structures are either JSON objects, JSON arrays or the JSON root.
  */
sealed trait JsonContext {

  /** Determines the specific type of a value and writes the value into the stream.
    *
    * @param value  value to be written into the stream
    * @param streamWriter instance of the stream writer
    */
  protected def determineSimpleValue(value: Any, streamWriter: JsonStreamWriter): Unit = {
    value match {
      case x: JBool => streamWriter.writeBooleanValue(x.value)
      case x: JString => streamWriter.writeStringValue(x.s)
      case x: JDecimal => streamWriter.writeValue(x.num)
      case x: JDouble => streamWriter.writeValue(x.num)
      case x: JInt => streamWriter.writeValue(x.num)
      case x: JLong => streamWriter.writeValue(x.num)
      case x: JNull.type => streamWriter.writeNull()
      case x: JNothing.type => streamWriter.writeNull()
      case x: Boolean => streamWriter.writeBooleanValue(x)
      case x: String => streamWriter.writeStringValue(x)
      case x: Option[Any] => this.determineSimpleValue(x.getOrElse(JNull), streamWriter)
      case x if x == null => streamWriter.writeNull()
      case x => streamWriter.writeValue(x)
    }
  }
}


/** Represents the JSON root. */
trait JsonRootContext extends JsonContext


/** Represents the enclosing JSON array.
  *
  * @param streamWriter instance of the output stream writer
  */
private[json] class JsonArrayContext(streamWriter: JsonStreamWriter) extends JsonContext {
  // indicates whether or not the JSON is generated in a nested context to prevent overlapping JSON structures
  // this is, for example, the case if a nested object or array is currently transformed to JSON and written to the stream
  private var isInNestedContext = false

  /** Appends a JSON objects to the JSON array.
    *
    * The resulting JSON string is directly streamed by the stream writer.
    *
    * @param generator generator function to describe the internal structure of the JSON object
    */
  def appendObject(generator: JsonObjectContext => Unit) = {
    if (!isInNestedContext) {
      val objectContext = new JsonObjectContext(streamWriter)
      isInNestedContext = true
      streamWriter.startObject()
      generator(objectContext)
      streamWriter.endObject()
      isInNestedContext = false
    }
  }


  /** Appends a JSON array to the current JSON array.
    *
    * The resulting JSON will consist of nested JSON Array structures.
    * The resulting JSON string is directly streamed by the stream writer.
    *
    * @param generator generator function to describe the internal structure of the JSON array
    */
  def appendArray(generator: JsonArrayContext => Unit) = {
    if (!isInNestedContext) {
      isInNestedContext = true
      val arrayContext = new JsonArrayContext(streamWriter)
      streamWriter.startArray()
      generator(arrayContext)
      streamWriter.endArray()
      isInNestedContext = false
    }
  }


  /** Appends a primitive value to the JSON array.
    *
    * The resulting JSON string is directly streamed by the stream writer.
    *
    * @param value value to be appended
    */
  def appendValue(value: Any): Unit = {
    if (!isInNestedContext) {
      value match {
        case x: JArray => this.appendArray({ ctx =>
          x.arr foreach { v => ctx.appendValue(v) }
        })
        case x: JObject => this.appendObject({ ctx =>
          x.obj foreach { v => ctx.appendValue(v._1, v._2) }
        })
        case x => determineSimpleValue(x, streamWriter)
      }
    }
  }
}


/** Represents the enclosing JSON object.
  *
  * @param streamWriter instance of the output stream writer
  */
private[json] class JsonObjectContext(streamWriter: JsonStreamWriter) extends JsonContext {
  // indicates whether or not the JSON is generated in a nested context to prevent overlapping JSON structures
  // this is, for example, the case if a nested object or array is currently transformed to JSON and written to the stream
  private var isInNestedContext = false

  /** Appends a key-value pair to the JSON object.
    *
    * The resulting JSON string is directly streamed by the stream writer.
    *
    * @param key unescaped key to be appended
    * @param value primitive value to be appended
    */
  def appendValue(key: String, value: Any): Unit = {
    if (!isInNestedContext) {
      value match {
        case x: JObject => this.appendObject(key) { ctx =>
          x.obj foreach { v => ctx.appendValue(v._1, v._2) }
        }
        case x: JArray => this.appendArray(key) { ctx =>
          x.arr foreach { v => ctx.appendValue(v) }
        }
        case x =>
          streamWriter.writeEscapedKey(key)
          determineSimpleValue(x, streamWriter)
      }
    }
  }


  /** Appends a key-value pair to the JSON object where the value is a JSON array.
    *
    * The resulting JSON string is directly streamed by the stream writer.
    *
    * @param key unescaped key to be appended
    * @param generator generator function for describing the JSON array's structure
    */
  def appendArray(key: String)(generator: JsonArrayContext => Unit): Unit = {
    if (!isInNestedContext) {
      val arrayContext = new JsonArrayContext(streamWriter)
      isInNestedContext = true
      streamWriter.writeEscapedKey(key)
      streamWriter.startArray()
      generator(arrayContext)
      streamWriter.endArray()
      isInNestedContext = false
    }
  }


  /** Appends a key-value pair to the JSON object where the value is a JSON object itself.
    *
    * The resulting JSON string is directly streamed by the stream writer.
    * The structure of the resulting JSON string will represent nested JSON objects.
    *
    * @param key unescaped key to be appended
    * @param generator generator function for describing the JSON objects's structure
    */
  def appendObject(key: String)(generator: JsonObjectContext => Unit): Unit = {
    if (!isInNestedContext) {
      isInNestedContext = true
      val objectContext = new JsonObjectContext(streamWriter)

      streamWriter.writeEscapedKey(key)
      streamWriter.startObject()
      generator(objectContext)
      streamWriter.endObject()
      isInNestedContext = false
    }
  }


  /** Adds a key-value pair to the JSON object specifying the original type of the object represented as JSON.
    *
    * The key value pair has the following structure: "$type$":"[typeInfo]".
    * The resulting JSON string is directly streamed by the stream writer.
    *
    * @param typeInfo the type name of the original object
    */
  def appendTypeInformation(typeInfo: String) = {
    if (!isInNestedContext) {
      streamWriter.writeEscapedKey("$type$")
      streamWriter.writeStringValue(typeInfo)
    }
  }
}


/** Writes JSON as stream and offers an API that allows the creation of valid JSON only. */
sealed trait JsonBuilder extends JsonRootContext {
  protected val streamWriter: JsonStreamWriter

  /** Closes the output stream writer. */
  def close() = streamWriter.close()
}


/** API to create a JSON object.
  *
  * @param out output stream
  * @param generator generator function to describe the structure of the JSON object
  */
class JsonObjectBuilder(out: Writer)(generator: JsonObjectContext => Unit) extends JsonBuilder {
  // JSON stream writer instance to output the generated JSON string
  protected val streamWriter = new JsonStreamWriter(out)

  // object context instance for handling the creation of JSON objects
  private val objectContext = new JsonObjectContext(streamWriter)

  // write object to stream
  streamWriter.startObject()
  generator(objectContext)
  streamWriter.endObject()
}


/** API to create a JSON array.
  *
  * @param out output stream
  * @param generator generator function to describe the array structure
  */
class JsonArrayBuilder(out: Writer)(generator: JsonArrayContext => Unit) extends JsonBuilder {
  // JSON stream writer instance to output the generated JSON string
  protected val streamWriter = new JsonStreamWriter(out)

  // array context instance for handling the creation of JSON arrays
  private val arrayContext = new JsonArrayContext(streamWriter)

  // write array to stream
  streamWriter.startArray()
  generator(arrayContext)
  streamWriter.endArray()
}


/** API to create a primitive JSON value.
  *
  * @param out output stream
  * @param value value to be written to the output
  */
class JsonValueBuilder(out: Writer)(value: Any) extends JsonBuilder {
  // JSON stream writer instance to output the generated JSON string
  protected val streamWriter = new JsonStreamWriter(out)

  // array context instance for handling the creation of JSON arrays
  private val arrayContext = new JsonArrayContext(streamWriter)

  // object context instance for handling the creation of JSON objects
  private val objectContext = new JsonObjectContext(streamWriter)

  // determine correct value type and write to stream
  value match {
    case x: JArray =>
      streamWriter.startArray()
      x.arr foreach { v => arrayContext.appendValue(v) }
      streamWriter.endArray()

    case x: JObject =>
      streamWriter.startObject()
      x.obj foreach { v => objectContext.appendValue(v._1, v._2) }
      streamWriter.endObject()

    case x => determineSimpleValue(x, streamWriter)
  }
}

