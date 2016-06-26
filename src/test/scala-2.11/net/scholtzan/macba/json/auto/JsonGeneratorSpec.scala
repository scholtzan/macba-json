package net.scholtzan.macba.json.auto

import net.scholtzan.macba.json.auto.JsonGenerator._
import net.scholtzan.macba.json.writer.StringBuilderWriter
import org.scalatest.{FlatSpec, Matchers}

/** Contains unit tests for `JsonGenerator`. */
class JsonGeneratorSpec extends FlatSpec with Matchers {
  "JsonGenerator" should "generate valid boolean" in {
    val writer = new StringBuilderWriter()
    writeJson(true, writer)

    writer.toString shouldEqual "true"
  }


  "JsonGenerator" should "generate valid null" in {
    toJsonString(null) shouldEqual "null"
  }


  "JsonGenerator" should "generate valid option" in {
    case class A(a: Float, b: Option[String])
    val x: scala.Float = 23

    toJsonString(A(x, Some("foo"))) shouldEqual """{"a":23.0,"b":"foo"}"""
  }


  "JsonGenerator" should "generate valid option None" in {
    case class A(a: Float, b: Option[Int])
    val x: scala.Float = 23

    toJsonString(A(x, None)) shouldEqual """{"a":23.0,"b":null}"""
  }


  "JsonGenerator" should "generate valid Sequence" in {
    toJsonString(Seq(1,2,3)) shouldEqual "[1,2,3]"
  }


  "JsonGenerator" should "generate valid Array" in {
    toJsonString(Array(1,2,3)) shouldEqual "[1,2,3]"
  }



  "JsonGenerator" should "generate valid List" in {
    toJsonString(List(1,2,3)) shouldEqual "[1,2,3]"
  }


  "JsonGenerator" should "generate valid Vector" in {
    toJsonString(Vector(1,2,3)) shouldEqual "[1,2,3]"
  }


  "JsonGenerator" should "generate valid BigDecimal" in {
    val x = BigDecimal(231.123)

    toJsonString(x) shouldEqual "231.123"
  }


  "JsonGenerator" should "generate valid BigInt" in {
    val x = BigInt(231)

    toJsonString(x) shouldEqual "231"
  }


  "JsonGenerator" should "generate valid Byte" in {
    val x: Byte = 2

    toJsonString(x) shouldEqual "2"
  }


  "JsonGenerator" should "generate valid Char" in {
    val x: Char = 'a'

    toJsonString(x) shouldEqual """"a""""
  }


  "JsonGenerator" should "generate valid Double" in {
    val x: Double = 123.123

    toJsonString(x) shouldEqual "123.123"
  }


  "JsonGenerator" should "generate valid Short" in {
    val x: Short = 1

    toJsonString(x) shouldEqual "1"
  }


  "JsonGenerator" should "generate valid Long" in {
    val x: Double = 123.123

    toJsonString(x) shouldEqual "123.123"
  }


  "JsonGenerator" should "generate valid Float" in {
    case class A(a: Float)
    val x: scala.Float = 23

    toJsonString(A(x)) shouldEqual """{"a":23.0}"""
  }


  "JsonGenerator" should "generate valid JSON from class" in {
    class ACl(a: Float) {
      val abc = a
    }

    val x: scala.Float = 23

    toJsonString(new ACl(x)) shouldEqual """{"abc":23.0}"""
  }


  "JsonGenerator" should "generate valid JSON with types" in {
    trait Shape

    case class Rectangle(width: Double, height: Double) extends Shape

    case class Ellipse(width: Double, height: Double) extends Shape

    toJsonStringWithTypes(Rectangle(3, 4)) shouldEqual
      """{"$type$":"net.scholtzan.macba.json.auto.JsonGeneratorSpec.Rectangle","width":3.0,"height":4.0}"""
  }


  "JsonGenerator" should "generate valid string" in {
    val s = Array.fill(10000)("x").mkString

    toJsonString(s) shouldEqual '"' + s + '"'
  }


  "JsonGenerator" should "have working direct to string function" in {
    toJsonString(true) shouldEqual "true"
  }


  "JsonGenerator" should "generate valid JSON object" in {
    case class User(name: String, age: Int)

    toJsonString(User("John", 32)) shouldEqual """{"name":"John","age":32}"""
  }


  "JsonGenerator" should "generate valid nested JSON object" in {
    case class Address(street: String, active: Boolean)
    case class User(name: String, age: Int, address: Address)

    val data = User("John", 32, Address("Road 1", active = true))

    toJsonString(data) shouldEqual """{"name":"John","age":32,"address":{"street":"Road 1","active":true}}"""
  }


  "JsonGenerator" should "generate valid nested JSON object with types" in {
    case class Address(street: String, active: Boolean)
    case class User(name: String, age: Int, address: Address)

    val data = User("John", 32, Address("Road 1", active = true))

    toJsonStringWithTypes(data) shouldEqual """{"$type$":"net.scholtzan.macba.json.auto.JsonGeneratorSpec.User","name":"John","age":32,"address":{"$type$":"net.scholtzan.macba.json.auto.JsonGeneratorSpec.Address","street":"Road 1","active":true}}"""
  }


  "JsonGenerator" should "generate valid sequence of JSON objects" in {
    case class Address(street: String, active: Boolean)
    case class User(name: String, age: Int, addresses: Seq[Address])

    val data = User("John", 32, Seq(Address("Road 1", active = true), Address("Main 1", active = false)))

    toJsonString(data) shouldEqual """{"name":"John","age":32,"addresses":[{"street":"Road 1","active":true},{"street":"Main 1","active":false}]}"""
  }
}
