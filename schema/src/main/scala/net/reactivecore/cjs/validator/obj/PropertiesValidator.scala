package net.reactivecore.cjs.validator.obj

import io.circe.JsonObject
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.util.VectorMap
import net.reactivecore.cjs.validator.{
  ValidationContext,
  ValidationProvider,
  ValidationResult,
  ValidationState,
  Validator
}
import net.reactivecore.cjs.restriction.ValidatingField

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

object PropertiesValidator {
  implicit val provider: ValidationProvider[ValidatingField[VectorMap[String, Schema], PropertiesValidator]] =
    ValidationProvider.forField[VectorMap[String, Schema], PropertiesValidator] { (origin, properties) =>
      val propertySchemas = properties.map { case (key, schema) =>
        key -> schema.validator(origin.enterObject(key))
      }
      PropertiesValidator(propertySchemas)
    }
}
