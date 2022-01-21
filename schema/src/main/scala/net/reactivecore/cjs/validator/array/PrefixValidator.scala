package net.reactivecore.cjs.validator.array

import io.circe.Json
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

case class PrefixValdiator(prefix: Vector[Validator]) extends ArrayValidator {

  override def validateArrayStateful(state: ValidationState, array: Vector[Json])(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val zipped = array.zip(prefix)
    val violations = zipped.flatMap { case (json, validator) =>
      validator.validateWithoutEvaluated(state, json).violations
    }
    val updatedState = state.copy(
      evaluatedIndices = state.evaluatedIndices ++ zipped.indices
    )
    updatedState -> ValidationResult(violations)
  }

  override def children: Vector[Validator] = prefix
}
