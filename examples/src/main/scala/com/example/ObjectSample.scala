package com.example

import io.circe.Json
import net.reactivecore.cjs.resolver.Downloader
import net.reactivecore.cjs.{Loader, Schema}

object ObjectSample extends App {
  val schemaCode =
    """
      |{
      |  "type": "object",
      |  "properties": {
      |    "userName": {
      |      "$ref": "#/$defs/user"
      |    },
      |    "age": {
      |      "$ref": "#/$defs/age"
      |    }
      |  },
      |  "required": ["userName", "age"],
      |  "$defs": {
      |    "user": {
      |       "type": "string",
      |       "minLength": 3
      |     },
      |     "age": {
      |       "type": "number"
      |     }
      |  }
      |}
      |""".stripMargin

  val validator = Loader.empty.fromJson(schemaCode).right.get

  def test(s: Json): Unit = {
    val result = validator.validate(s)
    println(s"Result of ${s}: ${result}")
  }

  test(Json.fromString("wrongType"))
  test(
    Json.obj(
      "userName" -> Json.fromString("Bob"),
      "age" -> Json.fromInt(42)
    )
  )

  // Missing UserName
  test(
    Json.obj(
      "age" -> Json.fromInt(42)
    )
  )

  // Age has Wrong type
  test(
    Json.obj(
      "userName" -> Json.fromString("Bob"),
      "age" -> Json.fromBoolean(false)
    )
  )

}
