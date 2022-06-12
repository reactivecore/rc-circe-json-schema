package net.reactivecore.cjs.validator.array

import io.circe.Json
import net.reactivecore.cjs.{Schema, SchemaOrigin}
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

/** Validator for List Items */
case class ItemValidator(
    underlying: Validator,
    prefixSize: Int
) extends ArrayValidator {
  override def validateArrayStateful(state: ValidationState, array: Vector[Json])(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val withoutPrefix = array.zipWithIndex.drop(prefixSize)
    val violations = withoutPrefix.flatMap { case (element, _) =>
      underlying.validateWithoutEvaluated(state, element).violations
    }
    val updatedState = state.copy(
      evaluatedIndices = state.evaluatedIndices ++ withoutPrefix.map(_._2)
    )
    updatedState -> ValidationResult(violations)
  }

  override def children: Vector[Validator] = Vector(underlying)
}

object ItemValidator {
  def apply(origin: SchemaOrigin, schema: Schema, prefixSize: Int): ItemValidator = {
    ItemValidator(schema.validator(origin), prefixSize)
  }
}
