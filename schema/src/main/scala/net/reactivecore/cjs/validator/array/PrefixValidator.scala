package net.reactivecore.cjs.validator.array

import io.circe.Json
import net.reactivecore.cjs.{Schema, SchemaOrigin}
import net.reactivecore.cjs.validator.{
  ValidationContext,
  ValidationProvider,
  ValidationResult,
  ValidationState,
  Validator,
  Violation
}
import net.reactivecore.cjs.restriction.ValidatingField

case class PrefixValidator(prefix: Vector[Validator]) extends ArrayValidator {

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

object PrefixValidator {

  def apply(origin: SchemaOrigin, prefix: Vector[Schema]): PrefixValidator = {
    val prefixValidators = prefix.zipWithIndex.map { case (schema, idx) =>
      schema.validator(origin.enterArray(idx))
    }
    PrefixValidator(prefixValidators)
  }

  implicit val provider: ValidationProvider[ValidatingField[Vector[Schema], PrefixValidator]] =
    ValidationProvider.forField[Vector[Schema], PrefixValidator](PrefixValidator.apply)
}
