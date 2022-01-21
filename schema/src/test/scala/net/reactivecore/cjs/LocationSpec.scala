package net.reactivecore.cjs

import io.circe.{Json, JsonObject}
import io.circe.syntax._

class LocationSpec extends TestBase {
  it should "encode into an empty object if nothing is in there" in {
    Location().asJson shouldBe Json.obj()
  }
}
