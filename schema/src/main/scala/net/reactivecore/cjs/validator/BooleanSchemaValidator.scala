package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.resolver.JsonPointer

case class BooleanSchemaValidator(origin: SchemaOrigin, value: Boolean) extends SchemaValidator {

  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    if (value) {
      (state, ValidationResult.success)
    } else {
      (state, ValidationResult.violation(json, s"False Boolean Schema"))
    }
  }
}
