package net.scholtzan.macba.json.format

import java.io.Writer
import net.scholtzan.macba.json.api._
import net.scholtzan.macba.json.api._


/** Contains methods for defining custom serialization rules. */
trait JsonWriterFormat[T] {

  /** Writes customly formatted JSON to the writer.
    *
    * @param value value to be customly written
    * @param writer output writer
    */
  def writeJson(value: T, writer: Writer): Unit
}


/** Used for defining custom serialization rules for specific types. */
object JsonWriterFormat {

  /** Defines the custom serialization logic for a specific value type to be serialized to a JSON array.
    *
    * @param f function that describes the serialization using a partially applied `JsonArrayBuilder`
    * @tparam T type to be serialized
    */
  def writeArray[T](f: (T, (JsonArrayContext => Unit) => JsonArrayBuilder) => JsonBuilder): JsonWriterFormat[T] = new JsonWriterFormat[T] {
    override def writeJson(value: T, writer: Writer): Unit = {
      val builder = (new JsonArrayBuilder(_: Writer)(_: (JsonArrayContext => Unit))).curried
      f(value, builder(writer))
    }
  }


  /** Defines the custom serialization logic for a specific value type to be serialized to a JSON object.
    *
    * @param f function that describes the serialization using a partially applied `JsonObjectBuilder`
    * @tparam T type to be serialized
    */
  def writeObject[T](f: (T, (JsonObjectContext => Unit) => JsonObjectBuilder) => JsonBuilder): JsonWriterFormat[T] = new JsonWriterFormat[T] {
    override def writeJson(value: T, writer: Writer): Unit = {
      val builder = (new JsonObjectBuilder(_: Writer)(_: (JsonObjectContext => Unit))).curried
      f(value, builder(writer))
    }
  }


  /** Defines the custom serialization logic for a specific value type to be serialized to a simple JSON value.
    *
    * @param f function that describes the serialization using a partially applied `JsonValueBuilder`
    * @tparam T type to be serialized
    */
  def writeValue[T](f: (T, Any => JsonValueBuilder) => JsonBuilder): JsonWriterFormat[T] = new JsonWriterFormat[T] {
    override def writeJson(value: T, writer: Writer): Unit = {
      val builder = (new JsonValueBuilder(_: Writer)(_: Any)).curried
      f(value, builder(writer))
    }
  }
}
