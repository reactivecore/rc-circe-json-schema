package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.util.{Codecs, VectorMap}
import net.reactivecore.cjs.validator.obj._
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.validator.ValidationProvider

case class ObjectRestriction(
    properties: OValidatingField[VectorMap[String, Schema], PropertiesValidator] = None,
    required: OValidatingField[Seq[String], RequiredValidator] = None,
    additionalProperties: OValidatingField[Schema, AdditionalPropertiesValidator] = None,
    dependentRequired: OValidatingField[VectorMap[String, Vector[String]], DependentRequiredValidator] = None,
    dependentSchemas: OValidatingField[VectorMap[String, Schema], DependentSchemasValidator] = None,
    unevaluatedProperties: OValidatingField[Schema, UnevaluatedItemsValidator] = None,
    minProperties: OValidatingField[Long, SimpleValidator.MinProperties] = None,
    maxProperties: OValidatingField[Long, SimpleValidator.MaxProperties] = None,
    propertyNames: OValidatingField[Schema, PropertyNamesValidator] = None,
    patternProperties: OValidatingField[VectorMap[String, Schema], PatternPropertiesValidator] = None
)

object ObjectRestriction {
  implicit lazy val codec: Codec.AsObject[ObjectRestriction] = Codecs.withoutNulls(semiauto.deriveCodec[ObjectRestriction])

  implicit val validationProvider: ValidationProvider[ObjectRestriction] =
    ValidationProvider.visitingSequental[ObjectRestriction]
}
