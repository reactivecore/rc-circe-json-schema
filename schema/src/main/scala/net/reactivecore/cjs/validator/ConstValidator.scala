package net.reactivecore.cjs.validator

import io.circe.Json

case class ConstValidator(expected: Json) extends SimpleContextFreeValidator(s"Const ${expected}") {
  override def isValid(json: Json): Boolean = {
    json == this.expected
  }
}
