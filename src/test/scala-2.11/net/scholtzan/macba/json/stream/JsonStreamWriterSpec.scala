package net.scholtzan.macba.json.stream

import net.scholtzan.macba.json.writer.StringBuilderWriter
import org.scalatest.{Matchers, FlatSpec}


/** Contains unit tests for `JsonStreamWriter` */
class JsonStreamWriterSpec extends FlatSpec with Matchers {
  "JsonStreamWriter" should "generate valid true boolean value" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    streamWriter.writeBooleanValue(true)
    streamWriter.close()

    out.toString shouldEqual "true"
  }


  "JsonStreamWriter" should "generate valid integer value" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    val i: Int = 12

    streamWriter.writeValue(i)
    streamWriter.close()

    out.toString shouldEqual "12"
  }


  "JsonStreamWriter" should "generate valid double value" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    val i: Double = 12.34

    streamWriter.writeValue(i)
    streamWriter.close()

    out.toString shouldEqual "12.34"
  }


  "JsonStreamWriter" should "generate valid null value" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)

    streamWriter.writeNull()
    streamWriter.close()

    out.toString shouldEqual "null"
  }


  "JsonStreamWriter" should "generate valid string value" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)

    streamWriter.writeStringValue("foobar")
    streamWriter.close()

    out.toString shouldEqual "\"foobar\""
  }


  "JsonStreamWriter" should "generate valid escaped string value" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)

    streamWriter.writeStringValue("""\r\n""")
    streamWriter.close()

    out.toString shouldEqual "\"\\r\\n\""
  }


  "JsonStreamWriter" should "generate valid empty JSON object" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    streamWriter.startObject()
    streamWriter.endObject()
    streamWriter.close()

    out.toString shouldEqual "{}"
  }


  "JsonStreamWriter" should "generate valid JSON object" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    streamWriter.startObject()
    streamWriter.writeEscapedKey("foo")
    streamWriter.writeValue(123)
    streamWriter.writeEscapedKey("bar")
    streamWriter.writeNull()
    streamWriter.endObject()
    streamWriter.close()

    out.toString shouldEqual """{"foo":123,"bar":null}"""
  }


  "JsonStreamWriter" should "generate valid nested JSON object" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    streamWriter.startObject()
    streamWriter.writeEscapedKey("foo")
    streamWriter.writeValue(123)
    streamWriter.writeEscapedKey("bar")
    streamWriter.writeNull()
    streamWriter.writeEscapedKey("pal")
    streamWriter.startObject()
    streamWriter.writeEscapedKey("test")
    streamWriter.writeBooleanValue(true)
    streamWriter.endObject()
    streamWriter.endObject()
    streamWriter.close()

    out.toString shouldEqual """{"foo":123,"bar":null,"pal":{"test":true}}"""
  }


  "JsonStreamWriter" should "generate valid empty JSON array" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    streamWriter.startArray()
    streamWriter.endArray()
    streamWriter.close()

    out.toString shouldEqual "[]"
  }


  "JsonStreamWriter" should "generate valid JSON array" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    streamWriter.startArray()
    streamWriter.writeValue(123)
    streamWriter.writeNull()
    streamWriter.writeStringValue("bar")
    streamWriter.endArray()
    streamWriter.close()

    out.toString shouldEqual """[123,null,"bar"]"""
  }


  "JsonStreamWriter" should "generate valid nested JSON array" in {
    val out = new StringBuilderWriter()
    val streamWriter = new JsonStreamWriter(out)
    streamWriter.startArray()
    streamWriter.writeValue(123)
    streamWriter.writeNull()
    streamWriter.startArray()
    streamWriter.writeValue(1.34)
    streamWriter.writeBooleanValue(false)
    streamWriter.endArray()
    streamWriter.endArray()
    streamWriter.close()

    out.toString shouldEqual """[123,null,[1.34,false]]"""
  }
}
