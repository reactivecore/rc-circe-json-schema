package net.reactivecore.cjs.validator.obj

import io.circe.JsonObject
import net.reactivecore.cjs.util.VectorMap
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

case class PropertiesValidator(validators: VectorMap[String, Validator]) extends ObjectValidator {
  override def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val violations = (validators
      .collectFirst {
        case (key, validator)
            if json(key).toVector.flatMap(validator.validateWithoutEvaluated(state, _).violations).nonEmpty =>
          json(key).toVector.flatMap(validator.validateWithoutEvaluated(state, _).violations)
      })
      .getOrElse(Nil)
    val updatedState = state.copy(
      evaluatedProperties = state.evaluatedProperties ++ validators.keySet
    )
    updatedState -> ValidationResult(violations)
  }

  override def children: Vector[Validator] = validators.toVector.map(_._2)
}
