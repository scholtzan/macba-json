package net.scholtzan.macba.json.stream

import java.io.Writer

/** Contains static data used in `JsonStreamWriter` instances. */
private object JsonStreamWriter {
  // characters to be escaped
  val escapedChars = Map(
    '\n' -> "\\n",
    '\t' -> "\\t",
    '\r' -> "\\r",
    '\b' -> "\\b",
    '\f' -> "\\f",
    '"' -> "\\\"")
}


/** Writes JSON into a stream. */
class JsonStreamWriter(outputWriter: Writer) {
  // JSON output writer
  val out = outputWriter

  // track if there are tokens before the current one to add commas
  private var isFirstToken = true

  /** Adds the token to the stream to indicate a JSON array start. */
  def startArray() = {
    writePrefix()
    out.write('[')
    isFirstToken = true
  }


  /** Adds the token for indicating the JSON array end to the stream. */
  def endArray() = {
    out.write(']')
  }


  /** Adds the token for indicating the JSON object start to the stream. */
  def startObject() = {
    writePrefix()
    out.write('{')
    isFirstToken = true
  }


  /** Adds the token for indicating the JSON object end to the stream. */
  def endObject() = {
    out.write('}')
  }


  /** Adds a value to the buffer representing the key of a key/value pair without escaping. */
  def writeRawKey(value: String) = {
    out.write(value)
    isFirstToken = true
  }


  /** Adds a value to the stream representing the key of a key/value pair with escaping. */
  def writeEscapedKey(value: String) = {
    writePrefix()
    writeEscapedString(value)

    out.write(':')
    isFirstToken = true
  }


  /** Add a boolean value to the stream.
    *
    * @param value value to be added to the stream
    */
  def writeBooleanValue(value: Boolean): Unit = {
    writePrefix()

    if (value) {
      out.write(Array('t','r','u','e'), 0, 4)
    } else {
      out.write(Array('f','a','l','s','e'), 0, 5)
    }
  }


  /** Add value to the stream.
    *
    * @param value value to be added to the stream
    */
  def writeValue(value: Any): Unit = {
    writePrefix()
    out.write(value.toString)
  }


  /** Add a string value to the stream.
    *
    * @param value value to be added to the stream
    */
  def writeStringValue(value: String): Unit = {
    writePrefix()
    writeEscapedString(value)
  }


  /** Add a null value to the stream. */
  def writeNull() = {
    writePrefix()
    out.write(Array('n','u','l','l'), 0, 4)
  }


  /** Escape a string value and write the escaped string into the stream.
    *
    * @param str string to be escaped and added to the stream
    */
  private def writeEscapedString(str: String) = {
    out.write('"')

    var i = 0
    var last = 0
    val cStr = str.toCharArray
    val strLen = cStr.length

    // iterate through all characters
    while(i < strLen) {
      val c = str.charAt(i)

      // check for characters that need to be escaped
      if (c <= '"' && c != ' ') {
        if (JsonStreamWriter.escapedChars.keySet.contains(c)) {
          if (last < i) {
            out.write(cStr, last, i - last)
            last = i + 1
          }

          out.write(JsonStreamWriter.escapedChars(c))
        }
      }

      i += 1
    }

    if (last < strLen) {
      out.write(cStr, last, strLen - last)
    }

    out.write('"')
  }


  /** Check if a comma needs to be added before the value. */
  private def writePrefix() = {
    if (!isFirstToken) {
      out.write(',')
    }

    isFirstToken = false
  }


  /** Called after the last bit of information has been added to close the stream. */
  def close() = {
    out.close()
  }


  /** Returns the target output writer. */
  def unwrap() = {
    out
  }
}
