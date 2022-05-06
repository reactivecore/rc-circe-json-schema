package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.SchemaContext
import net.reactivecore.cjs.resolver.JsonPointer

case class BooleanSchemaValidator(context: SchemaContext, value: Boolean) extends SchemaValidator {

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
