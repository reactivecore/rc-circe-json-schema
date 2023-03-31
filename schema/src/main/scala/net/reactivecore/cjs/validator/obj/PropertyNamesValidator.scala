package net.reactivecore.cjs.validator.obj

import io.circe.{Json, JsonObject}
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.validator.{
  ValidationContext,
  ValidationProvider,
  ValidationResult,
  ValidationState,
  Validator,
  Violation
}
import net.reactivecore.cjs.restriction.ValidatingField

case class PropertyNamesValidator(validator: Validator) extends ObjectValidator {
  override def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    state -> (json.keys
      .collectFirst {
        case name if validator.validateWithoutEvaluated(state, Json.fromString(name)).isFailure =>
          validator.validateWithoutEvaluated(state, Json.fromString(name))
      })
      .getOrElse(ValidationResult.success)
  }
}

object PropertyNamesValidator {
  implicit val provider: ValidationProvider[ValidatingField[Schema, PropertyNamesValidator]] =
    ValidationProvider.forField[Schema, PropertyNamesValidator] { (origin, schema) =>
      PropertyNamesValidator(schema.validator(origin))
    }
}
