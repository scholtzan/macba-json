package net.scholtzan.macba.json.writer

import org.scalatest.{Matchers, FlatSpec}


/** Contains unit tests for `StringBuilderWriter`. */
class StringBuilderWriterSpec extends FlatSpec with Matchers {
  "StringBuilderWriter" should "generate a valid string with append" in {
    val writer = new StringBuilderWriter
    writer.append('a')
    writer.append(Array('b', 'c', 'd'))

    writer.toString shouldEqual "abcd"
  }


  "StringBuilderWriter" should "write a valid string" in {
    val writer = new StringBuilderWriter
    writer.write("foo")
    writer.write(Array('b', 'a', 'r'), 0, 3)
    writer.flush() // should do nothing

    writer.toString shouldEqual "foobar"
  }
}
