package net.reactivecore.cjs.validator.obj

import io.circe.{Json, JsonObject}
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.validator.provider.ValidationProvider
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

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
  implicit val provider = ValidationProvider.forField[Schema, PropertyNamesValidator] { (origin, schema) =>
    PropertyNamesValidator(schema.validator(origin))
  }
}
