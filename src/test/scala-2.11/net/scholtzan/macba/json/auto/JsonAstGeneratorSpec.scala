package net.scholtzan.macba.json.auto

import org.json4s.JsonAST._
import org.scalatest.{FlatSpec, Matchers}
import JsonAstGenerator._


/** Contains unit tests for `JsonAstGenerator` */
class JsonAstGeneratorSpec extends FlatSpec with Matchers {
  "JsonAstGenerator" should "generate JBool" in {
    createJsonAst(true) shouldEqual JBool(true)
  }


  "JsonAstGenerator" should "generate JNull" in {
    createJsonAst(null) shouldEqual JNull
  }


  "JsonAstGenerator" should "generate JString" in {
    createJsonAst("foo") shouldEqual JString("foo")
  }


  "JsonAstGenerator" should "generate JDouble" in {
    val x: Double = 123.132

    createJsonAst(x) shouldEqual JDouble(123.132)
  }


  "JsonAstGenerator" should "generate JDecimal" in {
    val x = BigDecimal(123.132)

    createJsonAst(x) shouldEqual JDecimal(123.132)
  }


  "JsonAstGenerator" should "generate JLong" in {
    val x: Long = 12333333

    createJsonAst(x) shouldEqual JLong(12333333)
  }


  "JsonAstGenerator" should "generate JInt" in {
    createJsonAst(123) shouldEqual JInt(123)
  }


  "JsonAstGenerator" should "generate JObject" in {
    case class User(a: Int, b: String)

    createJsonAst(User(123, "foo")) shouldEqual JObject(List(JField("a", JInt(123)), JField("b", JString("foo"))))
  }


  "JsonAstGenerator" should "generate JArray from Seq" in {
    createJsonAst(Seq(1,2,3)) shouldEqual JArray(List(JInt(1), JInt(2), JInt(3)))
  }


  "JsonAstGenerator" should "generate JArray from List" in {
    createJsonAst(List(1,2,3)) shouldEqual JArray(List(JInt(1), JInt(2), JInt(3)))
  }


  "JsonAstGenerator" should "generate JArray from Vector" in {
    createJsonAst(Vector(1,2,3)) shouldEqual JArray(List(JInt(1), JInt(2), JInt(3)))
  }


  "JsonAstGenerator" should "generate nested JObject" in {
    case class User(a: Int, b: Seq[Boolean])

    createJsonAst(User(123, Seq(true, false))) shouldEqual JObject(List(JField("a", JInt(123)), JField("b", JArray(List(JBool(true), JBool(false))))))
  }
}
