package net.reactivecore.cjs.validator

import io.circe.{Json, parser}
import net.reactivecore.cjs.resolver.RefUri
import net.reactivecore.cjs.{DocumentValidator, Schema, TestBase}

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
        validator.validate(json) shouldBe 'success
      } else {
        validator.validate(json) shouldBe 'failure
      }
    }
  }

  "simple recursion" should "work" in {
    val schema = parser.parse(simpleRecursion).forceRight.as[Schema].forceRight
    val validator = schema.emptyResolve.forceRight

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
