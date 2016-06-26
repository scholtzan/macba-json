package net.scholtzan.macba.json.auto

import java.io.Writer
import net.scholtzan.macba.json.stream.JsonStreamWriter

import scala.language.experimental.macros
import scala.math.ScalaNumber
import scala.reflect.macros.blackbox


/** Contains macros for automatically determining the structure of a passed object
  * and it's corresponding JSON structure.
  */
object JsonGenerator {

  /** Macro definition for method to stream JSON string representation of a specific value.
    *
    * @param obj value to be transformed into a JSON string
    * @param writer output writer
    * @tparam T type of the passed value
    */
  def writeJson[T](obj: T, writer: Writer): Unit = macro writeJsonImpl[T]


  /** Macro implementation to create and stream JSON string representing the passed value.
    *
    * @param c macro context passed implicitly
    * @param obj AST representation of the value to be transformed into a JSON string
    * @param writer AST representation of the output writer
    * @tparam T type of the passed value
    */
  def writeJsonImpl[T](c: blackbox.Context)(obj: c.Expr[T], writer: c.Expr[Writer]) = {
    import c.universe._

    q"""
      val x = new net.scholtzan.macba.json.stream.JsonStreamWriter($writer)

      writeJsonToStream($obj, x)
    """
  }


  /** Macro definition for transforming a value recursively into a JSON string stream.
    *
    * @param obj value to be transformed into a JSON string
    * @param s output writer
    * @tparam T type of the passed value
    */
  def writeJsonToStream[T](obj: T, s: JsonStreamWriter): Unit = macro writeJsonToStreamImpl[T]


  /** Macro implementation for streaming the JSON string.
    *
    * @param c macro context passed implicitly
    * @param obj AST representation of the value to be transformed into a JSON string
    * @param s AST representation of the JSON output writer
    * @tparam T type of the passed value
    */
  def writeJsonToStreamImpl[T](c: blackbox.Context)(obj: c.Expr[T], s: c.Expr[JsonStreamWriter]) = {
    stream(c)(obj, s, includeTypes = false)
  }


  /** Macro definition for method to stream JSON string representation of a specific value including its types.
    *
    * @param obj value to be transformed into a JSON string
    * @param writer output writer
    * @tparam T type of the passed value
    */
  def writeJsonWithTypes[T](obj: T, writer: Writer): Unit = macro writeJsonWithTypesImpl[T]


  /** Macro implementation to create JSON string including value types representing the passed value.
    *
    * @param c macro context passed implicitly
    * @param obj AST representation of the value to be transformed into a JSON string
    * @param writer AST representation of the output writer
    * @tparam T type of the passed value
    */
  def writeJsonWithTypesImpl[T](c: blackbox.Context)(obj: c.Expr[T], writer: c.Expr[Writer]) = {
    import c.universe._

    q"""
      val x = new net.scholtzan.macba.json.stream.JsonStreamWriter($writer)

      streamJsonWithTypesDirectly($obj, x)
    """
  }


  /** Macro definition for method to create the JSON string of a specific value.
    *
    * @param obj value to be transformed into a JSON string
    * @tparam T type of the passed value
    * @return JSON string
    */
  def toJsonString[T](obj: T): String = macro toJsonStringImpl[T]


  /** Macro implementation to create the JSON string representing the passed value.
    *
    * @param c macro context passed implicitly
    * @param obj AST representation of the value to be transformed into a JSON string
    * @tparam T type of the passed value
    */
  def toJsonStringImpl[T](c: blackbox.Context)(obj: c.Expr[T]) = {
    import c.universe._

    q"""
      val out = new net.scholtzan.macba.json.writer.StringBuilderWriter()
      val x = new net.scholtzan.macba.json.stream.JsonStreamWriter(out)

      writeJsonToStream($obj, x)

      out.toString
    """
  }


  /** Macro definition for method to create the JSON string of a specific value including type information.
    *
    * @param obj value to be transformed into a JSON string
    * @tparam T type of the passed value
    * @return JSON string
    */
  def toJsonStringWithTypes[T](obj: T): String = macro toJsonStringWithTypesImpl[T]


  /** Macro implementation to create the JSON string representing the passed value including type information.
    *
    * @param c macro context passed implicitly
    * @param obj AST representation of the value to be transformed into a JSON string
    * @tparam T type of the passed value
    */
  def toJsonStringWithTypesImpl[T](c: blackbox.Context)(obj: c.Expr[T]) = {
    import c.universe._

    q"""
      val out = new net.scholtzan.macba.json.writer.StringBuilderWriter()
      val x = new net.scholtzan.macba.json.stream.JsonStreamWriter(out)

      writeJsonToStreamWithTypes($obj, x)

      out.toString
    """
  }


  /** Macro definition for transforming a value recursively into a JSON string stream including its types.
    *
    * @param obj value to be transformed into a JSON string
    * @param s output stream writer
    * @tparam T type of the passed value
    */
  def writeJsonToStreamWithTypes[T](obj: T, s: JsonStreamWriter): Unit = macro writeJsonToStreamWithTypesImpl[T]


  /** Macro implementation for streaming the JSON string with types.
    *
    * @param c macro context passed implicitly
    * @param obj AST representation of the value to be transformed into a JSON string
    * @param s AST representation of the JSON output writer
    * @tparam T type of the passed value
    */
  def writeJsonToStreamWithTypesImpl[T](c: blackbox.Context)(obj: c.Expr[T], s: c.Expr[JsonStreamWriter]) = {
    stream(c)(obj, s, includeTypes = true)
  }


  /** Determines the appropriate JSON values for representing the passed value as JSON string.
    *
    * The JSON representation is written into a stream writer.
    *
    * @param c macro context passed implicitly
    * @param obj value to be represented as JSON string
    * @param s output writer
    * @param includeTypes true if the JSON string should include type information
    * @tparam T type of the passed value
    * @return abstract syntax tree
    */
  private def stream[T: c.WeakTypeTag](c: blackbox.Context)(obj: c.Expr[T], s: c.Expr[JsonStreamWriter], includeTypes: Boolean): c.Tree = {
    import c.universe._

    // get type of object
    val objectType = obj.actualType

    // check if there is an implicit serialization rule for this type
    val tq"$implicitFormatType" = tq"net.scholtzan.macba.json.format.JsonWriterFormat[$objectType]"

    try { // implicit rule exists
      val implicitFormatFunction = c.typecheck(q"implicitly[$implicitFormatType]")

      q"$implicitFormatFunction.writeJson($obj, $s.unwrap)"
    } catch { // no implicit rule
      case _: Throwable =>
        // determine the appropriate value type
        obj match {
          case x if x.actualType <:< typeOf[Null] => // null value
            q"$s.writeNull()"

          case x if x.actualType <:< typeOf[Option[Any]] => // option value
            if (includeTypes) {
              q"""
                if ($x.isDefined) {
                  writeJsonToStreamWithTypes($x.get, $s)
                } else {
                  $s.writeNull()
                }
              """
            } else {
              q"""
                if ($x.isDefined) {
                  writeJsonToStream($x.get, $s)
                } else {
                  $s.writeNull()
                }
              """
            }

          case x if x.actualType.typeSymbol == typeOf[Boolean].typeSymbol =>
            q"$s.writeBooleanValue($x)"

          case x if x.actualType <:< typeOf[ScalaNumber] =>
            q"$s.writeValue($x)"

          case x if x.actualType.typeSymbol == typeOf[String].typeSymbol => // string value
            q"$s.writeStringValue($x)" // string will be escaped by output writer

          case x if x.actualType.typeSymbol == typeOf[Char].typeSymbol =>
            q"$s.writeStringValue($x.toString)"

          case x if x.actualType <:< typeOf[Array[Any]] ||
                    x.actualType <:< typeOf[Seq[Any]] ||
                    x.actualType.typeSymbol == typeOf[Array[Any]].typeSymbol => // sequences of any type
            if (includeTypes) {
              q"""
                $s.startArray()
                $x foreach { o =>
                  writeJsonToStreamWithTypes(o, $s)
                }
                $s.endArray()
              """
            } else {
              q"""
                $s.startArray()
                $x foreach { o =>
                  writeJsonToStream(o, $s)
                }
                $s.endArray()
              """
            }

          case x if x.actualType <:< typeOf[Object] => // all other objects where the structure is to be determined
            // get all members and methods
            val methods: Iterable[c.Symbol] = x.tree.tpe.decls

            // get accessible values and variables only
            val fields = methods.filter(sym => sym.isMethod && sym.asMethod.isAccessor)

            // create the JSON by recursively generating key value pairs
            if (includeTypes) {
              q"""
                $s.startObject()
                $s.writeRawKey(${"\"$type$\":\"" + x.actualType.typeSymbol.fullName + '"'})

                ..${
                  fields.map { field =>
                    q"""
                      $s.writeRawKey(${",\"" + field.name.toString + "\":"})
                      writeJsonToStreamWithTypes($obj.${field.name.toTermName}, $s)
                    """
                  }
                }

                $s.endObject()
              """
            } else {
              var prefix = ""

              q"""
                $s.startObject()
                ..${
                  fields.map { field =>
                    q"""
                      $s.writeRawKey(${prefix + '"' + field.name.toString + "\":"})
                      ${prefix = ","}
                      writeJsonToStream($obj.${field.name.toTermName}, $s)
                    """
                  }
                }

                $s.endObject()
              """
            }

          case x => // all other unhandled types
            q"$s.writeValue($x)"
        }
    }
  }
}
