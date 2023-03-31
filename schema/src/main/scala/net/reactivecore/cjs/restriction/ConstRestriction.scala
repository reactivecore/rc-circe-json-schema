package net.reactivecore.cjs.restriction

import io.circe._
import net.reactivecore.cjs.validator.{ConstValidator, ValidationProvider, Validator}

/**
  * Restricts to a constant value.
  * Note: custom serialization, as null is a valid value.
  */
case class ConstRestriction(
    const: OValidatingField[Json, ConstValidator] = None
)

object ConstRestriction {
  implicit val encoder: Encoder.AsObject[ConstRestriction] = Encoder.AsObject {
    case ConstRestriction(None)        => JsonObject()
    case ConstRestriction(Some(value)) => JsonObject("const" -> value.value)
  }

  implicit val decoder: Decoder[ConstRestriction] = Decoder.decodeJsonObject.map { o =>
    ConstRestriction(o("const").map(ValidatingField(_)))
  }

  implicit val codec: Codec.AsObject[ConstRestriction] = Codec.AsObject.from(decoder, encoder)

  implicit val validationProvider: ValidationProvider[ConstRestriction] = ValidationProvider.visitingSequental[ConstRestriction]
}
