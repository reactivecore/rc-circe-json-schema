package net.reactivecore.cjs.validator.array

import io.circe.Json
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

case class ContainsValidator(
    underlying: Validator,
    min: Int,
    max: Option[Int]
) extends ArrayValidator {
  override def validateArrayStateful(state: ValidationState, array: Vector[Json])(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val validIndices = array.zipWithIndex.collect {
      case (element, index) if underlying.validateWithoutEvaluated(state, element).isSuccess => index
    }
    if (validIndices.size >= min && max.forall(validIndices.size <= _)) {
      val updatedState = state.copy(
        evaluatedIndices = state.evaluatedIndices ++ validIndices
      )
      updatedState -> ValidationResult.success
    } else {
      state -> ValidationResult.violation(
        Json.fromValues(array),
        s"Contains mismatch, expected min: ${min} to ${max.getOrElse("inf")}, got ${validIndices.size}"
      )
    }
  }
}
