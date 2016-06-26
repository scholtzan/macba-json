package net.scholtzan.macba.json.format

import net.scholtzan.macba.json.auto.JsonGenerator._
import net.scholtzan.macba.json.writer.StringBuilderWriter
import org.scalatest.{Matchers, FlatSpec}


/** Contains unit tests for `JsonWriterFormat`. */
class JsonWriterFormatSpec extends FlatSpec with Matchers {
  "JsonWriterFormat" should "transform integers to JSON arrays" in {
    implicit val form = JsonWriterFormat.writeArray[Int]{ (x, arrayBuilder) => {
      arrayBuilder( ctx => {
        ctx.appendValue(x)
      })
    }}

    val x: Int = 123
    val wr = new StringBuilderWriter
    writeJson(x, wr)
    wr.toString shouldEqual "[123]"
  }


  "JsonWriterFormat" should "transform integers to JSON objects" in {
    implicit val form = JsonWriterFormat.writeObject[Int]{ (x, objBuilder) => {
      objBuilder( ctx => {
        ctx.appendValue("key", x)
      })
    }}

    val x: Int = 123
    val wr = new StringBuilderWriter
    writeJson(x, wr)
    wr.toString shouldEqual """{"key":123}"""
  }


  "JsonWriterFormat" should "generate valid integer value as string" in {
    implicit val form = JsonWriterFormat.writeValue[Int]{ (x, valueBuilder) => {
      valueBuilder(x.toString)
    }}

    val x: Int = 123
    val wr = new StringBuilderWriter
    writeJson(x, wr)
    wr.toString shouldEqual """"123""""
  }


/*
  The following unit tests only work when Joda Time is added as dependency.
 */

//  "JsonWriterFormat" should "generate valid date" in {
//    implicit val form = JsonWriterFormat.writeValue[DateTime]{ (date, valueBuilder) => {
//      valueBuilder(date.getDayOfMonth + "." + date.getMonthOfYear + "." + date.getYear)
//    }}
//
//    val x: Int = 123
//    val wr = new StringBuilderWriter
//    writeJson(x, wr)
//    wr.toString shouldEqual """123"""
//  }
//
//
//  "JsonWriterFormat" should "generate valid date as object" in {
//    implicit val form = JsonWriterFormat.writeObject[DateTime]{ (date, objectBuilder) => {
//      objectBuilder( ctx => {
//        ctx.appendValue("day", date.getDayOfMonth)
//        ctx.appendValue("month", date.getMonthOfYear)
//        ctx.appendValue("year", date.getYear)
//      })
//    }}
//
//    val x: Int = 123
//    val wr = new StringBuilderWriter
//    writeJson(new DateTime(), wr)
//    wr.toString shouldEqual """{"day":25,"month":4,"year":2016}"""
//  }
}
