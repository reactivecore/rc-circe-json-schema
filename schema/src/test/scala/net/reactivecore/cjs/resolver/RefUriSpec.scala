package net.reactivecore.cjs.resolver

import io.circe.Json
import io.circe.syntax._
import net.reactivecore.cjs.TestBase

import java.net.URI

class RefUriSpec extends TestBase {
  "empty" should "work" in {
    RefUri().toString shouldBe ""
    RefUri.forceString("") shouldBe RefUri()
  }

  "stripping fragment" should "work" in {
    RefUri
      .fromString("http://foobar.com/bar#1234")
      .forceRight
      .copy(
        fragment = None
      )
      .toString shouldBe "http://foobar.com/bar"
  }

  "resolving" should "work" in {
    RefUri
      .fromString("http://foobar.com:1234/baz/biz/buz#abc")
      .forceRight
      .resolve(RefUri.fromString("../bimba#cde").forceRight)
      .toString shouldBe "http://foobar.com:1234/baz/bimba#cde"
  }

  "appendPathFragment" should "work" in {
    RefUri.forceString("http://foo.bar").appendPathFragment("abcd/def/x").toString shouldBe "http://foo.bar#/abcd/def/x"
    RefUri
      .forceString("http://foo.bar#existing")
      .appendPathFragment("abcd/def/x")
      .toString shouldBe "http://foo.bar#/abcd/def/x"
    RefUri
      .forceString("http://foo.bar#/existing/bar")
      .appendPathFragment("abcd/def/x")
      .toString shouldBe "http://foo.bar#/existing/bar/abcd/def/x"
    RefUri().appendPathFragment("a/b/c").toString shouldBe "#/a/b/c"
  }

  "json" should "work" in {
    val sample = RefUri.fromString("http://localhost/abdd").forceRight
    sample.asJson shouldBe Json.fromString(sample.toString)
    sample.asJson.as[RefUri] shouldBe Right(sample)
  }

  "urns" should "be handled" in {
    val urn = "urn:uuid:deadbeef-1234-00ff-ff00-4321feebdaed"
    val ref = RefUri.forceString(urn)
    ref.toUri shouldBe new URI(urn)
    val subResolved = ref.resolve(RefUri.forceString("#/$defs/bar"))
    subResolved.toString shouldBe (urn + "#/$defs/bar")
  }

  it should "shouldBe supported with fragments" in {
    val urn = "urn:uuid:deadbeef-1234-00ff-ff00-4321feebdaed#/foo/bar"
    val ref = RefUri.forceString(urn)
    ref.toUri shouldBe new URI(urn)
    ref.toString shouldBe urn
  }
}
