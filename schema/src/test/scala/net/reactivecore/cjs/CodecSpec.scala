package net.reactivecore.cjs

import io.circe.{Json, JsonObject, parser}
import io.circe.syntax._
import org.apache.commons.io.IOUtils

import java.nio.charset.StandardCharsets

class CodecSpec extends TestBase {

  val examples = Seq(
    "/samples/schema-website/address.schema.json",
    "/samples/schema-website/arrays.schema.json",
    "/samples/schema-website/fstab.schema.json"
  )

  for {
    example <- examples
  } it should s"parse and serialize the example ${example}" in {
    val bytes = IOUtils.resourceToString(example, StandardCharsets.UTF_8).ensuring(_ != null)
    val parsedJson = parser.parse(bytes).forceRight
    val parsed = parsedJson.as[Schema].forceRight
    val encoded = parsed.asJson
    ensureJsonEqual(parsedJson, encoded)
  }
}
