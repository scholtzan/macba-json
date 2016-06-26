package net.scholtzan.macba.json.auto

import org.json4s.JsonAST.JValue
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Contains macros for automatically determining the structure of a passed object
  * and it's corresponding JSON AST structure.
  *
  * The JSON AST is based on json4s's JSON AST to provide compatibility with other libraries.
  */
object JsonAstGenerator {

  /** Macro definition for method to get JSON AST representation of a specific type.
    *
    * @param obj value to be transformed into a JSON AST
    * @tparam T type of the passed value
    * @return JSON AST representation of the passed value
    */
  def createJsonAst[T](obj: T): JValue = macro createJsonAstImpl[T]


  /** Macro implementation that inspects passed value and creates its JSON AST representation.
    *
    * @param c macro context passed implicitly
    * @param obj AST of the value to be transformed into a JSON AST
    * @tparam T type of the passed value to be transformed
    * @return AST of the JSON AST representation
    */
  def createJsonAstImpl[T](c: blackbox.Context)(obj: c.Expr[T]): c.Tree = {
    import c.universe._

    // determine the type and its corresponding JSON AST value
    obj match {
      case x if x.actualType <:< typeOf[Int] =>
        q"org.json4s.JsonAST.JInt($x)"

      case x if x.actualType.typeSymbol == typeOf[Double].typeSymbol =>
        q"org.json4s.JsonAST.JDouble($x)"

      case x if x.actualType.typeSymbol == typeOf[BigDecimal].typeSymbol =>
        q"org.json4s.JsonAST.JDecimal($x)"

      case x if x.actualType.typeSymbol == typeOf[Long].typeSymbol =>
        q"org.json4s.JsonAST.JLong($x)"

      case x if x.actualType.typeSymbol == typeOf[String].typeSymbol =>
        q"org.json4s.JsonAST.JString($x)"

      case x if x.actualType.typeSymbol == typeOf[Boolean].typeSymbol =>
        q"org.json4s.JsonAST.JBool($x)"

      case x if x.actualType <:< typeOf[Null] =>
        q"org.json4s.JsonAST.JNull"

      case x if x.actualType <:< typeOf[Seq[Any]]  =>
        q"org.json4s.JsonAST.JArray($obj.map(createJsonAst(_)).toList)"

      case x if x.actualType <:< typeOf[List[Any]] =>
        q"org.json4s.JsonAST.JArray($obj.map(createJsonAst(_)))"

      case x =>  // unknown object or data type

        // get all members and methods
        val methods: Iterable[c.Symbol] = obj.tree.tpe.decls

        // get accessible values and variables only
        val fields = methods.filter(sym => sym.isMethod && sym.asMethod.isAccessor)

        // create the JSON AST by recursively generating key value pairs
        val jsonObjectAst = q"""
          org.json4s.JsonAST.JObject(List(..${fields.map { field =>
            q"""org.json4s.JsonAST.JField(${field.name.toString}, createJsonAst($obj.${field.name.toTermName}))"""
          }}))
        """

        jsonObjectAst
    }
  }
}
