package net.reactivecore.cjs.validator

import io.circe.parser
import net.reactivecore.cjs.{DocumentValidator, DownloaderMock, Schema, TestBase}
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
    val resolvedSchema = DocumentValidator.parseAndResolveJson(schema, downloader).forceRight
  }

  it should "figure out when it's ok" in new Env {
    resolvedSchema.validate(parser.parse(sampleOk).forceRight) shouldBe 'success
  }

  for { bad <- Seq(sampleBad1, sampleBad2) } {
    it should s"figure out ${bad} is not ok" in new Env {
      resolvedSchema.validate(parser.parse(bad).forceRight) shouldBe 'failure
    }
  }
}
