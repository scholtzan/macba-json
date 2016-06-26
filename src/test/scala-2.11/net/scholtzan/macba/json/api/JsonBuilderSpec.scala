package net.scholtzan.macba.json.api

import net.scholtzan.macba.json.auto.JsonAstGenerator
import net.scholtzan.macba.json.writer.StringBuilderWriter
import org.json4s.JsonAST._
import org.scalatest.{FlatSpec, Matchers}
import JsonAstGenerator._


/** Contains unit tests for `JsonBuilder`. */
class JsonBuilderSpec extends FlatSpec with Matchers {
  // simple values

  "JsonBuilder" should "generate valid integer value" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(88)

    out.toString shouldEqual "88"
  }


  "JsonBuilder" should "generate valid boolean value" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(false)

    out.toString shouldEqual "false"
  }


  "JsonBuilder" should "generate valid None" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(None)

    out.toString shouldEqual "null"
  }


  "JsonBuilder" should "generate valid Some" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(Some(123))

    out.toString shouldEqual "123"
  }


  "JsonBuilder" should "generate valid double value" in {
    val out = new StringBuilderWriter()
    val d: Double = 123.23
    val builder = new JsonValueBuilder(out)(d)

    out.toString shouldEqual "123.23"
  }


  "JsonBuilder" should "generate valid escaped string value" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)("""f"oo\nba\tr""")

    out.toString shouldEqual """"f\"oo\nba\tr""""
  }

  // JSON objects

  "JsonBuilder" should "create valid JSON object" in {
    val out = new StringBuilderWriter()
    val builder = new JsonObjectBuilder(out)( ctx => {
      ctx.appendValue("foo", 123)
      ctx.appendValue("bar", true)
    })

    out.toString shouldEqual """{"foo":123,"bar":true}"""
  }


  "JsonBuilder" should "create valid JSON object with type information" in {
    val out = new StringBuilderWriter()
    val builder = new JsonObjectBuilder(out)( ctx => {
      ctx.appendTypeInformation("TestClass")
      ctx.appendValue("foo", 123)
      ctx.appendValue("bar", true)
    })

    out.toString shouldEqual """{"$type$":"TestClass","foo":123,"bar":true}"""
  }


  // JSON arrays

  "JsonBuilder" should "create valid JSON array" in {
    val out = new StringBuilderWriter()

    val builder = new JsonArrayBuilder(out)( ctx => {
      ctx.appendValue(123)
      ctx.appendValue(null)
      ctx.appendValue("fooo")
    })

    out.toString shouldEqual """[123,null,"fooo"]"""
  }


  "JsonBuilder" should "create valid nested JSON array with objects" in {
    val out = new StringBuilderWriter()
    val builder = new JsonArrayBuilder(out)( ctx => {
      ctx.appendObject { ctx =>
        ctx.appendValue("foo", false)
        ctx.appendArray("arr"){ ctb =>
          ctb.appendValue(12)
          ctx.appendValue("a", 3) // ignored
          ctb.appendValue(false)
        }
        ctx.appendValue("bar", 12.5)
      }
      ctx.appendValue(1)
    })

    out.toString shouldEqual """[{"foo":false,"arr":[12,false],"bar":12.5},1]"""
  }


  "JsonBuilder" should "create valid nested JSON arrays" in {
    val out = new StringBuilderWriter()
    val builder = new JsonArrayBuilder(out)( ctx => {
      ctx.appendArray { cta =>
        cta.appendValue(123)
        ctx.appendValue("asdfsfd") // ignored
        cta.appendValue(23)
      }

      ctx.appendArray { cta =>
        cta.appendValue(false)
        cta.appendValue(true)
      }
    })

    out.toString shouldEqual "[[123,23],[false,true]]"
  }


  "JsonBuilder" should "create valid nested JSON objects" in {
    val out = new StringBuilderWriter()
    new JsonObjectBuilder(out)( ctx => {
      ctx.appendObject("foo"){ ctb =>
        ctb.appendValue("a", 123)
        ctb.appendObject("obj"){ cth =>
          cth.appendValue("b", true)
          cth.appendValue("v", 23)
        }
        ctb.appendValue("asdf", 34)
      }
    })

    out.toString shouldEqual """{"foo":{"a":123,"obj":{"b":true,"v":23},"asdf":34}}"""
  }


  // JSON AST

  "JsonBuilder" should "generate valid true boolean from JBool" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JBool(true))

    out.toString shouldEqual "true"
  }


  "JsonBuilder" should "generate valid integer value from JInt" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JInt(234))

    out.toString shouldEqual "234"
  }


  "JsonBuilder" should "generate valid string value from JString" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JString("foobar"))

    out.toString shouldEqual "\"foobar\""
  }



  "JsonBuilder" should "generate valid escaped string value from JString" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JString("""f"oo\nba\tr"""))

    out.toString shouldEqual """"f\"oo\nba\tr""""
  }


  "JsonBuilder" should "generate null value from JNothing" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JNothing)

    out.toString shouldEqual """null"""
  }


  "JsonBuilder" should "generate null value from JNull" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JNothing)

    out.toString shouldEqual """null"""
  }


  "JsonBuilder" should "generate double value from JDouble" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JDouble(123.23))

    out.toString shouldEqual """123.23"""
  }


  "JsonBuilder" should "generate double value from JDecimal" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JDecimal(123.23))

    out.toString shouldEqual """123.23"""
  }


  "JsonBuilder" should "generate double value from JLong" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JLong(222222))

    out.toString shouldEqual """222222"""
  }


  "JsonBuilder" should "generate valid JSON object string from JObject" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JObject(List(
                        JField("foo", JInt(234)),
                        JField("bar", JString("ufta")),
                        JField("obj", JObject(List(
                            JField("a", JDecimal(123))
                        )))
                     )))

    out.toString shouldEqual """{"foo":234,"bar":"ufta","obj":{"a":123}}"""
  }


  "JsonBuilder" should "generate valid JSON object string with nested array from JObject" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JObject(List(
      JField("foo", JInt(234)),
      JField("bar", JArray(List(JInt(4), JBool(false)))),
      JField("obj", JObject(List(
        JField("a", JDecimal(123))
      )))
    )))

    out.toString shouldEqual """{"foo":234,"bar":[4,false],"obj":{"a":123}}"""
  }


  "JsonBuilder" should "generate valid JSON array string from JArray" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JArray(List(
                        JBool(true),
                        JArray(List(
                          JInt(234),
                          JDouble(123.21)
                        )),
                        JLong(234234)
                     )))

    out.toString shouldEqual "[true,[234,123.21],234234]"
  }


  "JsonBuilder" should "generate valid JSON array string with nested object from JArray" in {
    val out = new StringBuilderWriter()
    val builder = new JsonValueBuilder(out)(JArray(List(
      JBool(true),
      JArray(List(
        JInt(234),
        JDouble(123.21)
      )),
      JObject(List(JField("foo", JString("bar"))))
    )))

    out.toString shouldEqual """[true,[234,123.21],{"foo":"bar"}]"""
  }


  // mixed

  "JsonBuilder" should "create valid nested JSON arrays from manual definition and JArray" in {
    val out = new StringBuilderWriter()
    val builder = new JsonArrayBuilder(out)( ctx => {
      ctx.appendArray{ cta =>
        cta.appendValue(123)
        ctx.appendValue("asdfsfd") // ignored
        cta.appendValue(23)
      }

      ctx.appendValue(JArray(List(
        JBool(false), JBool(true)
      )))

      ctx.appendArray { ctg =>
        ctg.appendValue("foo")
      }
    })

    out.toString shouldEqual """[[123,23],[false,true],["foo"]]"""
  }


  "JsonBuilder" should "create valid nested JSON objects from manual definition and JArray" in {
    val out = new StringBuilderWriter()
    val builder = new JsonObjectBuilder(out)( ctx => {
      ctx.appendValue("foo", 654)
      ctx.appendValue("bar", JString("abcd"))
      ctx.appendValue("arr", JArray(List(
        JInt(234), JBool(true), JDecimal(456)
      )))
    })

    out.toString shouldEqual """{"foo":654,"bar":"abcd","arr":[234,true,456]}"""
  }


  "JsonBuilder" should "create combine manual and automatic generation" in {
    case class Address(street: String, number: Int)
    case class User(name: String, address: Address, age: Int)

    val out = new StringBuilderWriter()

    def generate(u: User) = {
      new JsonObjectBuilder(out)( ctx => {
        ctx.appendValue("name", u.name)
        ctx.appendValue("address", createJsonAst(u.address))
        ctx.appendValue("age", u.age + " years")
      })
    }

    generate(User("John", Address("Main Street", 1), 55))

    out.toString shouldEqual """{"name":"John","address":{"street":"Main Street","number":1},"age":"55 years"}"""
  }


  "JsonBuilder" should "JsonObjectBuilder" in {
    val out = new StringBuilderWriter()
    val builder = new JsonObjectBuilder(out)( ctx => {
      ctx.appendValue("foo", 654)
      ctx.appendValue("bar", JString("abcd"))
      ctx.appendValue("arr", JArray(List(
        JInt(234), JBool(true), JDecimal(456)
      )))
    })

    builder.close()

    out.toString shouldEqual """{"foo":654,"bar":"abcd","arr":[234,true,456]}"""
  }
}
