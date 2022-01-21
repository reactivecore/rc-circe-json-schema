package net.reactivecore.cjs.validator.obj

import io.circe.{Json, JsonObject}
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

trait ObjectValidator extends Validator {

  override final def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    json.asObject
      .map { obj =>
        validateStatefulObject(state, obj)
      }
      .getOrElse(state -> ValidationResult.success)
  }

  def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult)
}

trait StatelessValidator extends ObjectValidator {
  override def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    state -> validate(json)
  }

  def validate(json: JsonObject): ValidationResult
}

/** Simple validator which fails, wehn f is false */
abstract class SimpleValidator(name: String)(f: JsonObject => Boolean) extends StatelessValidator {
  override def validate(json: JsonObject): ValidationResult = {
    if (f(json)) {
      ValidationResult.success
    } else {
      ValidationResult.violation(Json.fromJsonObject(json), name)
    }
  }
}

object SimpleValidator {
  case class MinProperties(minProperties: Long)
      extends SimpleValidator("minProperties")({ obj =>
        obj.size >= minProperties
      })

  case class MaxProperties(maxProperties: Long)
      extends SimpleValidator("maxProperties")({ obj =>
        obj.size <= maxProperties
      })
}
