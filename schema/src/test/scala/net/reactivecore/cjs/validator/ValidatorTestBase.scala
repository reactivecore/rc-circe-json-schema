package net.reactivecore.cjs.validator

import io.circe.parser
import net.reactivecore.cjs.{DocumentValidator, DownloaderMock, Loader, Schema, TestBase}
import cats.implicits._

abstract class ValidatorTestBase extends TestBase {
  def testSchemaValidation(schema: String, test: String, isValid: Boolean): Unit = {
    val downloader = new DownloaderMock
    val documentValidator = Loader(downloader).fromJson(schema).forceRight
    val parsedSample = parser.parse(test).forceRight
    val violations = documentValidator.validate(parsedSample)
    if (isValid) {
      withClue(s"${test} should be valid") {
        violations.isSuccess shouldBe true
      }
    } else {
      withClue(s"${test} shouldNot be valid") {
        violations.isFailure shouldBe true
      }
    }
  }
}
