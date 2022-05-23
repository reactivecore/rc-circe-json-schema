package net.reactivecore.cjs

import io.circe.generic.semiauto
import io.circe.{Codec, Decoder, Encoder, Json}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.ValidationProvider

/** Human description of a Schema */
case class Description(
    title: Option[String] = None,
    description: Option[String] = None,
    default: Option[Json] = None, // According to spec, the default is just for documentation purposes
    `$comment`: Option[String] = None
)

object Description {
  implicit val codec: Codec.AsObject[Description] = Codecs.withoutNulls(semiauto.deriveCodec[Description])

  implicit val validationProvider: ValidationProvider[Description] = ValidationProvider.empty
}
