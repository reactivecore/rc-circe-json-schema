package net.reactivecore.cjs.validator.obj
import io.circe.JsonObject
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

case class AdditionalPropertiesValidator(additionalCheck: String => Boolean, validator: Validator)
    extends ObjectValidator {
  override def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val violations = json.toIterable
      .collectFirst {
        case (key, value) if additionalCheck(key) && validator.validateWithoutEvaluated(state, value).isFailure =>
          validator.validateWithoutEvaluated(state, value)
      }
      .getOrElse(ValidationResult.success)

    val updatedState = state.copy(
      evaluatedProperties = state.evaluatedProperties ++ json.keys.filter(additionalCheck)
    )

    updatedState -> violations
  }
}
