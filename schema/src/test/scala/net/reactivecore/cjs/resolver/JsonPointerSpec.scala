package net.reactivecore.cjs.resolver

import net.reactivecore.cjs.TestBase

class JsonPointerSpec extends TestBase {
  val sample = JsonPointer().enterObject("a").enterArray(0).enterObject("b")
  it should "easy concatenate" in {
    sample.toString shouldBe "/a/0/b"
  }

  it should "be easy to construct from Strings" in {
    JsonPointer.fromString(sample.toString) shouldBe Some(sample)
    JsonPointer.fromString("illegal") shouldBe None
  }

  it should "know if it starts somehow" in {
    JsonPointer().startsWith(JsonPointer()) shouldBe true
    sample.startsWith(JsonPointer.fromString("/a/0").get) shouldBe true
    sample.startsWith(JsonPointer.fromString("/a/1").get) shouldBe false
  }

  it should "escape properly" in {
    val sample = JsonPointer("With / Slash", "With ~ Tilde")
    val encoded = "/With ~1 Slash/With ~0 Tilde"
    sample.toString shouldBe encoded
    JsonPointer.fromString(encoded) shouldBe Some(sample)
  }
}
