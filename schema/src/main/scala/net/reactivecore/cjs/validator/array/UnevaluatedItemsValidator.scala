package net.reactivecore.cjs.validator.array

import io.circe.Json
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.validator._
import net.reactivecore.cjs.restriction.ValidatingField

case class UnevaluatedItemsValidator(underlying: Validator) extends ArrayValidator {
  override def validateArrayStateful(
      state: ValidationState,
      array: Vector[Json]
  )(implicit validationContext: ValidationContext): (ValidationState, ValidationResult) = {
    val unevaluated = array.zipWithIndex.collect {
      case (elem, index) if !(state.evaluatedIndices.contains(index)) => elem
    }
    val violations = unevaluated.flatMap { json =>
      underlying.validateWithoutEvaluated(state, json).violations
    }
    val updatedState = state.copy(
      evaluatedIndices = state.evaluatedIndices ++ array.indices
    )
    updatedState -> ValidationResult(violations)
  }

  override def precedence: Int = 2
}

object UnevaluatedItemsValidator {
  implicit val provider: ValidationProvider[ValidatingField[Schema, UnevaluatedItemsValidator]] = ValidationProvider.forField[Schema, UnevaluatedItemsValidator] { (origin, schema) =>
    UnevaluatedItemsValidator(schema.validator(origin))
  }
}
