package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.util.{Codecs, VectorMap}
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}
import net.reactivecore.cjs.validator.obj._
import net.reactivecore.cjs.Schema

import java.util.regex.Pattern

case class ObjectRestriction(
    properties: Option[VectorMap[String, Schema]] = None,
    required: Option[Vector[String]] = None,
    additionalProperties: Option[Schema] = None,
    dependentRequired: Option[VectorMap[String, Vector[String]]] = None,
    dependentSchemas: Option[VectorMap[String, Schema]] = None,
    unevaluatedProperties: Option[Schema] = None,
    minProperties: Option[Long] = None,
    maxProperties: Option[Long] = None,
    propertyNames: Option[Schema] = None,
    patternProperties: Option[VectorMap[String, Schema]] = None
)

object ObjectRestriction {
  implicit lazy val codec: Codec.AsObject[ObjectRestriction] = Codecs.withoutNulls(semiauto.deriveCodec)

  implicit val validationProvider: ValidationProvider[ObjectRestriction] = ValidationProvider.withOrigin {
    (origin, objectRestrictions) =>
      val regular = Validator.sequenceOfOpts(
        objectRestrictions.properties.map { properties =>
          val propertiesPath = origin.enterObject("properties")
          val propertySchemas = properties.map { case (key, schema) =>
            key -> schema.validator(propertiesPath.enterObject(key))
          }
          PropertiesValidator(propertySchemas)
        },
        objectRestrictions.required.map { required =>
          RequiredValidator(required)
        },
        objectRestrictions.minProperties.map { minProperties =>
          SimpleValidator.MinProperties(minProperties)
        },
        objectRestrictions.maxProperties.map { maxProperties =>
          SimpleValidator.MaxProperties(maxProperties)
        },
        objectRestrictions.propertyNames.map { schema =>
          val validator = schema.validator(origin)
          PropertyNamesValidator(validator)
        },
        objectRestrictions.additionalProperties.map { schema =>
          val validator = schema.validator(origin)
          val additionalCheck = isAdditionalProperty(objectRestrictions)
          AdditionalPropertiesValidator(additionalCheck, validator)
        },
        objectRestrictions.dependentRequired.map { dependentRequired =>
          DependentRequiredValidator(dependentRequired)
        },
        objectRestrictions.dependentSchemas.map { dependentSchemas =>
          val withSchemas = dependentSchemas.mapValues(_.validator(origin)).toMap
          DependentSchemasValidator(withSchemas)
        },
        objectRestrictions.patternProperties.map { patternProperties =>
          val regexSchemas: Vector[(Pattern, Validator)] = patternProperties.toVector.map { case (pattern, schema) =>
            Pattern.compile(pattern) -> schema.validator(origin)
          }
          PatternPropertiesValidator(regexSchemas)
        },
        objectRestrictions.unevaluatedProperties.map { unevaluatedProperties =>
          val validator = unevaluatedProperties.validator(origin)
          UnevaluatedItemsValidator(validator)
        }
      )

      regular
  }

  /** Returns true if a given property name is a additional property */
  private def isAdditionalProperty(objectRestriction: ObjectRestriction): String => Boolean = {
    val regularKeys: Set[String] = objectRestriction.properties.getOrElse(Vector.empty).map(_._1).toSet
    val patternPropertyMatcher = (objectRestriction.patternProperties
      .getOrElse(Vector.empty)
      .map { case (pattern, _) =>
        Pattern.compile(pattern)
      })
      .toVector
    key => !regularKeys.contains(key) && !patternPropertyMatcher.exists(p => p.matcher(key).find())
  }
}
