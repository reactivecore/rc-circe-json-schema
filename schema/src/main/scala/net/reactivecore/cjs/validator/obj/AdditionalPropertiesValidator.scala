package net.reactivecore.cjs.validator.obj
import io.circe.JsonObject
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.restriction.{ObjectRestriction, ValidatingField}
import net.reactivecore.cjs.validator.{
  ValidationContext,
  ValidationProvider,
  ValidationResult,
  ValidationState,
  Validator
}

import java.util.regex.Pattern

case class AdditionalPropertiesValidator(additionalCheck: String => Boolean, validator: Validator)
    extends ObjectValidator {
  override def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val violations = json.toIterable
      .collectFirst {
        case (key, value) if additionalCheck(key) && validator.validateWithoutEvaluated(state, value).isFailure =>
          validator.validateWithoutEvaluated(state, value)
      }
      .getOrElse(ValidationResult.success)

    val updatedState = state.copy(
      evaluatedProperties = state.evaluatedProperties ++ json.keys.filter(additionalCheck)
    )

    updatedState -> violations
  }
}

object AdditionalPropertiesValidator {
  implicit val provider
      : ValidationProvider[(ValidatingField[Schema, AdditionalPropertiesValidator], ObjectRestriction)] = {
    case (origin, (field, context)) =>
      val regularProperties: Set[String] = context.properties.map(_.value.keySet).getOrElse(Set.empty)
      val patternPropertyMatchers: Vector[Pattern] = context.patternProperties
        .map(_.value)
        .getOrElse(Vector.empty)
        .map { case (pattern, _) =>
          Pattern.compile(pattern)
        }
        .toVector

      /** Returns true if the given property is not evaluated using properties or patternProperties */
      def isAdditionalProperty(propertyName: String): Boolean = {
        !regularProperties.contains(propertyName) && !patternPropertyMatchers.exists(p =>
          p.matcher(propertyName).find()
        )
      }

      val validator = field.value.validator(origin)
      val additionalCheck = isAdditionalProperty(_)
      AdditionalPropertiesValidator(additionalCheck, validator)
  }
}
