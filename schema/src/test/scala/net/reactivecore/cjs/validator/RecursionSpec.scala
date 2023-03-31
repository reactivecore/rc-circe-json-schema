package net.reactivecore.cjs.validator

import io.circe.{Json, parser}
import net.reactivecore.cjs.resolver.RefUri
import net.reactivecore.cjs.{DocumentValidator, Loader, Schema, TestBase}

class RecursionSpec extends TestBase {

  val simpleRecursion =
    """
      |{
      |  "anyOf": [
      |    {
      |      "type": "null"
      |    },
      |    {
      |      "type": "object",
      |      "properties": {
      |        "child": {
      |          "$ref": "#"
      |        }
      |      },
      |      "required": ["child"]
      |    }
      |  ]
      |}
      |""".stripMargin

  private def testValdiation(
      validator: DocumentValidator,
      json: Json,
      mustSucceed: Boolean
  ): Unit = {
    withClue(s"Validating ${json} should succeed ${mustSucceed}") {
      if (mustSucceed) {
        validator.validate(json).isSuccess shouldBe true
      } else {
        validator.validate(json).isFailure shouldBe true
      }
    }
  }

  "simple recursion" should "work" in {
    val validator = Loader.empty.fromJson(simpleRecursion).forceRight

    validator.context.resolve(RefUri.forceString("#")).forceRight shouldBe validator.roots.head._2.validator

    testValdiation(validator, Json.Null, true)

    testValdiation(
      validator,
      Json.obj(
        "other" -> Json.Null
      ),
      false
    )

    testValdiation(
      validator,
      Json.obj(
        "child" -> Json.Null
      ),
      true
    )

    testValdiation(
      validator,
      Json.obj(
        "child" -> Json.obj(
          "other" -> Json.Null
        )
      ),
      false
    )

    testValdiation(
      validator,
      Json.obj(
        "child" -> Json.obj("child" -> Json.Null)
      ),
      true
    )
  }
}
