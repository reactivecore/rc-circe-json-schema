package com.example

import io.circe.Json
import net.reactivecore.cjs.Schema

/** Simple example validating a number. */
object Simple extends App {

  val schemaCode =
    """
      |{
      |  "type": "number",
      |  "minimum": 5,
      |  "maximum": 10
      |}
      |""".stripMargin

  val schema = Schema.parse(schemaCode).right.get

  val validator = schema.emptyResolve.right.get

  def test(s: Json): Unit = {
    val result = validator.validate(s)
    println(s"Result of ${s}: ${result}")
  }

  test(Json.fromBigDecimal(3))
  test(Json.obj("hello" -> Json.fromString("World")))
  test(Json.fromInt(11))
  test(Json.fromInt(7))
}
