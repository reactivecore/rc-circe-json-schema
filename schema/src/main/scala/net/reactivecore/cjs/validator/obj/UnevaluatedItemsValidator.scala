package net.reactivecore.cjs.validator.obj

import io.circe.JsonObject
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

case class UnevaluatedItemsValidator(underlying: Validator) extends ObjectValidator {
  override def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val violations = (json.toIterable
      .collectFirst {
        case (key, value)
            if !state.evaluatedProperties
              .contains(key) && underlying.validateWithoutEvaluated(state, value).isFailure =>
          underlying.validateWithoutEvaluated(state, value)
      })
      .getOrElse(ValidationResult.success)
    val updatedState = state.copy(
      evaluatedProperties = json.keys.toSet
    )
    (updatedState, violations)
  }

  override def precedence: Int = 2
}
