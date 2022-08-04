package net.reactivecore.cjs.validator

import io.circe.{Json, JsonNumber}
import net.reactivecore.cjs.{Loader, TestBase}

/** Extra test for tricky reference ordering */
class RefOrderSpec extends TestBase {
  // Note: copied from Test Suite
  val sample =
    """{
      |  "$comment" : "$id must be evaluated before $ref to get the proper $ref destination",
      |  "$id" : "/ref-and-id2/base.json",
      |  "$ref" : "#bigint",
      |  "$defs" : {
      |    "bigint" : {
      |      "$comment" : "canonical uri: /ref-and-id2/base.json/$defs/bigint; another valid uri for this location: /ref-and-id2/base.json#bigint",
      |      "$anchor" : "bigint",
      |      "maximum" : 10
      |    },
      |    "smallint" : {
      |      "$comment" : "canonical uri: /ref-and-id2#/$defs/smallint; another valid uri for this location: /ref-and-id2/#bigint",
      |      "$id" : "/ref-and-id2/",
      |      "$anchor" : "bigint",
      |      "maximum" : 2
      |    }
      |  }
      |}""".stripMargin

  it should "work use the first definition" in {
    val validator = Loader.empty.fromJson(sample).forceRight
    validator.validate(Json.fromInt(5)).violations shouldBe empty
  }
}
