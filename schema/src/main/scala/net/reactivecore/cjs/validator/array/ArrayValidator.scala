package net.reactivecore.cjs.validator.array

import io.circe.Json
import net.reactivecore.cjs.restriction.ValidatingField
import net.reactivecore.cjs.validator.{
  ValidationContext,
  ValidationProvider,
  ValidationResult,
  ValidationState,
  Validator,
  Violation
}

trait ArrayValidator extends Validator {
  final override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    json.asArray.map(validateArrayStateful(state, _)).getOrElse(state, ValidationResult.success)
  }

  def validateArrayStateful(state: ValidationState, array: Vector[Json])(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult)
}

trait StatelessArrayValidator extends ArrayValidator {
  override def validateArrayStateful(
      state: ValidationState,
      array: Vector[Json]
  )(implicit context: ValidationContext): (ValidationState, ValidationResult) = {
    state -> validate(array)
  }

  def validate(array: Vector[Json]): ValidationResult
}

abstract class SimpleValidator(name: String)(
    f: Vector[Json] => Boolean
) extends StatelessArrayValidator {

  override def validate(array: Vector[Json]): ValidationResult = {
    if (f(array)) {
      ValidationResult.success
    } else {
      ValidationResult.violation(Json.fromValues(array), name)
    }
  }
}

object SimpleValidator {
  case class MinItems(minItems: Long) extends SimpleValidator("minItems")(_.size >= minItems)
  case class MaxItems(maxItems: Long) extends SimpleValidator("maxItems")(_.size <= maxItems)

  case object Unique
      extends SimpleValidator("unique")({ array =>
        val result = array.size == array.distinct.size
        result
      }) {

    implicit def validationProvider: ValidationProvider[ValidatingField[Boolean, SimpleValidator.Unique.type]] =
      ValidationProvider.forField { case (_, value) =>
        if (value) {
          SimpleValidator.Unique
        } else {
          Validator.success
        }
      }
  }
}
