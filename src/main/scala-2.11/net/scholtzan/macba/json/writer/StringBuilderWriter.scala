package net.scholtzan.macba.json.writer

import java.io.Writer


/** Writes a stream into a `StringBuilder`. */
class StringBuilderWriter extends Writer with Serializable {
  // contains the final string
  private val builder = new StringBuilder()


  override def flush() = {}


  override def close() = {}


  /** Appends a char value to the string builder.
    *
    * @param value char value to be appended
    * @return instance of itself
    */
  override def append(value: Char): Writer = {
    builder.append(value)
    this
  }


  /** Appends an array of characters to the string builder.
    *
    * @param value char values as array to be appended
    * @param offset starting position for reading the passed char array
    * @param length number of characters read from the passed char array
    */
  override def write(value: Array[Char], offset: Int, length: Int) = this.builder.appendAll(value, offset, length)


  /** Appends a string value to the string builder.
    *
    * @param value string value to be appended
    */
  override def write(value: String) = this.builder.append(value)


  /** Returns the string value stored in the string builder.
    *
    * @return generated string
    */
  override def toString: String = this.builder.toString
}
