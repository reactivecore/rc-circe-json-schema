package net.reactivecore.cjs.restriction

import io.circe._
import net.reactivecore.cjs.validator.{ConstValidator, ValidationProvider, Validator}

/**
  * Restricts to a constant value.
  * Note: custom serialization, as null is a valid value.
  */
case class ConstRestriction(
    const: Option[Json] = None
)

object ConstRestriction {
  implicit val encoder: Encoder.AsObject[ConstRestriction] = Encoder.AsObject {
    case ConstRestriction(None)        => JsonObject()
    case ConstRestriction(Some(value)) => JsonObject("const" -> value)
  }

  implicit val decoder: Decoder[ConstRestriction] = Decoder.decodeJsonObject.map { o =>
    ConstRestriction(o("const"))
  }

  implicit val codec: Codec.AsObject[ConstRestriction] = Codec.AsObject.from(decoder, encoder)

  implicit val validationProvider: ValidationProvider[ConstRestriction] = ValidationProvider.instance {
    case ConstRestriction(Some(const)) => ConstValidator(const)
    case ConstRestriction(None)        => Validator.success
  }
}
