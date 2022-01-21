package net.reactivecore.cjs.validator.obj
import io.circe.JsonObject
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

import java.util.regex.Pattern

case class PatternPropertiesValidator(regexSchemas: Vector[(Pattern, Validator)]) extends ObjectValidator {
  override def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val violations = (for {
      (key, json) <- json.toIterable
      (pattern, validator) <- regexSchemas
      if pattern.matcher(key).find()
      violation <- validator.validateWithoutEvaluated(state, json).violations
    } yield violation).toVector

    val props = json.keys.toSet.filter { key =>
      regexSchemas.exists(_._1.matcher(key).find())
    }
    val updatedState = state.copy(
      evaluatedProperties = state.evaluatedProperties ++ props
    )
    (updatedState, ValidationResult(violations))
  }
}
