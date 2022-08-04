package com.example

import net.reactivecore.cjs.{DocumentValidator, Loader, Result}
import net.reactivecore.cjs.resolver.Downloader
import cats.implicits._
import io.circe.Json

object Downloaded extends App {

  // Test a downloaded Schema (using the Schema of JSON-Schema itself)

  val jsonSchemaUrl = "https://json-schema.org/draft/2020-12/schema"

  // Overriding to see what is downloaded.
  val downloader = new Downloader.JavaUrlDownloader {
    override def loadJson(url: String): Result[Json] = {
      println(s"Downloading ${url}...")
      val result = super.loadJson(url)
      result
    }
  }

  val validator = Loader(downloader).fromUrl(jsonSchemaUrl).right.get

  def test(s: Json): Unit = {
    val result = validator.validate(s)
    println(s"Result of ${s}: ${result}")
  }

  // A Boolean Schema
  test(
    Json.fromBoolean(true)
  )

  // An Object Schema
  test(
    Json.obj(
      "type" -> Json.fromString("object"),
      "required" -> Json.arr(
        Json.fromString("a"),
        Json.fromString("b")
      )
    )
  )

  // Numbers are not JSON Schemas
  test(
    Json.fromInt(3)
  )

  // Required requires values to be Strings, numbers are not allowed
  test(
    Json.obj(
      "type" -> Json.fromString("object"),
      "required" -> Json.arr(
        Json.fromInt(3),
        Json.fromInt(4)
      )
    )
  )
}
