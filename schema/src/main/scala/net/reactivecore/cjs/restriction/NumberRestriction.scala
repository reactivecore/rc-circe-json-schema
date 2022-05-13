package net.reactivecore.cjs.restriction

import io.circe.generic.semiauto
import io.circe.{Codec, Json}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.number.{
  ExclusiveMaximumValidator,
  ExclusiveMinimumValidator,
  MaximumValidator,
  MinimumValidator,
  MultipleOfValidator
}
import net.reactivecore.cjs.validator.provider.{SequenceValidationProvider, ValidationProvider}
import net.reactivecore.cjs.validator.{ValidationResult, Validator, Violation}

case class NumberRestriction(
    minimum: Option[SingleRestriction[BigDecimal, MinimumValidator]] = None,
    exclusiveMinimum: Option[SingleRestriction[BigDecimal, ExclusiveMinimumValidator]] = None,
    maximum: Option[SingleRestriction[BigDecimal, MaximumValidator]] = None,
    exclusiveMaximum: Option[SingleRestriction[BigDecimal, ExclusiveMaximumValidator]] = None,
    multipleOf: Option[SingleRestriction[BigDecimal, MultipleOfValidator]] = None
) {
  def isEmpty: Boolean = this == NumberRestriction()
}

object NumberRestriction {
  implicit lazy val codec: Codec.AsObject[NumberRestriction] = Codecs.withoutNulls(semiauto.deriveCodec)

  implicit lazy val validationProvider: ValidationProvider[NumberRestriction] =
    SequenceValidationProvider[NumberRestriction]
}
