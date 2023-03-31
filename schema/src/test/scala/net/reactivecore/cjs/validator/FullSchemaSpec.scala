package net.reactivecore.cjs.validator

import io.circe.parser
import net.reactivecore.cjs.{DocumentValidator, DownloaderMock, Loader, Schema, TestBase}
import cats.implicits._

/** Duplicates defs.json-Test-Suite Spec for better Debugging */
class FullSchemaSpec extends TestBase {
  val sampleOk = """{"$defs": {"foo": {"type": "integer"}}}"""
  val sampleBad1 = """{"type": 1}"""
  val sampleBad2 = """{"$defs": {"foo": {"type": 1}}}"""

  val schema =
    """
      |{"$ref": "https://json-schema.org/draft/2020-12/schema"}
      |""".stripMargin

  trait Env {
    val downloader = new DownloaderMock

    val documentValidator = Loader(downloader).fromJson(schema).forceRight
  }

  it should "figure out when it's ok" in new Env {
    documentValidator.validate(parser.parse(sampleOk).forceRight).isSuccess shouldBe true
  }

  for { bad <- Seq(sampleBad1, sampleBad2) } {
    it should s"figure out ${bad} is not ok" in new Env {
      documentValidator.validate(parser.parse(bad).forceRight).isFailure shouldBe true
    }
  }
}
