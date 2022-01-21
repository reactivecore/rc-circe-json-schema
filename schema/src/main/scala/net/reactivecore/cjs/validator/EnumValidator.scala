package net.reactivecore.cjs.validator

import io.circe.Json

case class EnumValidator(possibleValues: Vector[Json]) extends SimpleContextFreeValidator(s"Enum: ${possibleValues}") {
  override def isValid(json: Json): Boolean = {
    possibleValues.contains(json)
  }
}
