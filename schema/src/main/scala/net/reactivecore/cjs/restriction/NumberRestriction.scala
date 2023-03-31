package net.reactivecore.cjs.restriction

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.ValidationProvider
import net.reactivecore.cjs.validator.number._

case class NumberRestriction(
    minimum: OValidatingField[BigDecimal, MinimumValidator] = None,
    exclusiveMinimum: OValidatingField[BigDecimal, ExclusiveMinimumValidator] = None,
    maximum: OValidatingField[BigDecimal, MaximumValidator] = None,
    exclusiveMaximum: OValidatingField[BigDecimal, ExclusiveMaximumValidator] = None,
    multipleOf: OValidatingField[BigDecimal, MultipleOfValidator] = None
)

object NumberRestriction {
  implicit lazy val codec: Codec.AsObject[NumberRestriction] = Codecs.withoutNulls(semiauto.deriveCodec[NumberRestriction])

  implicit lazy val validationProvider: ValidationProvider[NumberRestriction] =
    ValidationProvider.visitingSequental[NumberRestriction]
}
