package net.reactivecore.cjs.validator

import io.circe.parser
import net.reactivecore.cjs.{DocumentValidator, DownloaderMock, Schema, TestBase}
import cats.implicits._

abstract class ValidatorTestBase extends TestBase {
  def testSchemaValidation(schema: String, test: String, isValid: Boolean): Unit = {
    val downloader = new DownloaderMock
    val resolvedSchema = DocumentValidator.parseAndResolveJson(schema, downloader).forceRight
    val parsedSample = parser.parse(test).forceRight
    val violations = resolvedSchema.validate(parsedSample)
    if (isValid) {
      withClue(s"${test} should be valid") {
        violations shouldBe 'success
      }
    } else {
      withClue(s"${test} shouldNot be valid") {
        violations shouldBe 'failure
      }
    }
  }
}
