package net.reactivecore.cjs.resolver

import cats.implicits._
import io.circe.parser
import net.reactivecore.cjs.{DownloaderMock, TestBase}

class ResolverSpec extends TestBase {
  trait Env {
    val downloader = new DownloaderMock()
    val resolver = new Resolver(downloader)
  }

  val json = parser
    .parse("""
             |{
             |            "$id": "http://localhost:1234/scope_change_defs1.json",
             |            "type" : "object",
             |            "properties": {"list": {"$ref": "baseUriChangeFolder/"}},
             |            "$defs": {
             |                "baz": {
             |                    "$id": "baseUriChangeFolder/",
             |                    "type": "array",
             |                    "items": {"$ref": "folderInteger.json"}
             |                }
             |            }
             |        }
             |""".stripMargin)
    .forceRight

  "resolve" should "work" in new Env {
    val mainId = RefUri.forceString("http://localhost:1234/scope_change_defs1.json")
    val resolved =
      resolver.resolve(json).forceRight
    val expected = Resolved(
      main = mainId,
      roots = Map(
        RefUri.forceString("http://localhost:1234/scope_change_defs1.json") -> json,
        RefUri.forceString("http://localhost:1234/baseUriChangeFolder/") -> parser
          .parse(
            """
              |{
              |  "$id" : "baseUriChangeFolder/",
              |  "type" : "array",
              |  "items" : {
              |    "$ref" : "folderInteger.json"
              |  }
              |}
              |""".stripMargin
          )
          .forceRight,
        RefUri.forceString("http://localhost:1234/baseUriChangeFolder/folderInteger.json") -> parser
          .parse(
            """
              |{
              |    "type": "integer"
              |}
              |
              |""".stripMargin
          )
          .forceRight
      )
    )
    resolved.roots.keySet shouldBe expected.roots.keySet
    resolved shouldBe expected
  }
}
