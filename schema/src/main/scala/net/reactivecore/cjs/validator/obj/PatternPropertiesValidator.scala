package net.reactivecore.cjs.validator.obj
import io.circe.JsonObject
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.util.VectorMap
import net.reactivecore.cjs.validator.{
  ValidationContext,
  ValidationProvider,
  ValidationResult,
  ValidationState,
  Validator,
  Violation
}

import java.util.regex.Pattern
import net.reactivecore.cjs.restriction.ValidatingField

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

object PatternPropertiesValidator {
  implicit val provider: ValidationProvider[ValidatingField[VectorMap[String, Schema], PatternPropertiesValidator]] =
    ValidationProvider.forField[VectorMap[String, Schema], PatternPropertiesValidator] { (origin, patternProperties) =>
      val regexSchemas: Vector[(Pattern, Validator)] = patternProperties.toVector.map { case (pattern, schema) =>
        Pattern.compile(pattern) -> schema.validator(origin)
      }
      PatternPropertiesValidator(regexSchemas)
    }
}
