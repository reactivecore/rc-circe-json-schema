package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.validator.Validator.Compound

/** Validator for Definitions. Note: the validator itself doesn't really validate, but contains validators */
case class DefinitionsValidator(underlying: Vector[Validator]) extends Compound(underlying) {

  override def touch(state: ValidationState): ValidationState = {
    underlying.foldLeft(state) { case (s, u) => u.touch(s) }
  }

  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    touch(state) -> ValidationResult.success
  }

  override def precedence: Int = {
    // Definitions like to set anchors, so we want them before
    -1
  }
}
