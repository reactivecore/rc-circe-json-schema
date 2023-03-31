package net.reactivecore.cjs.restriction

import io.circe.generic.semiauto
import io.circe.{Codec, Decoder, Encoder, Json}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{EnumValidator, ValidationProvider, Validator}
import net.reactivecore.cjs.DataTypeName

case class EnumRestriction(
    `enum`: OValidatingField[Vector[Json], EnumValidator] = None
)

object EnumRestriction {
  type TypeOrTypes = Either[DataTypeName, Vector[DataTypeName]]
  implicit val dataTypeVectorCodec: Codec[Vector[DataTypeName]] = Codec.from(
    Decoder.decodeVector[DataTypeName],
    Encoder.encodeVector[DataTypeName]
  )

  implicit val typeCodec: Codec[TypeOrTypes] = Codecs.disjunctEitherCodec[DataTypeName, Vector[DataTypeName]]
  implicit val codec: Codec.AsObject[EnumRestriction] = Codecs.withoutNulls(semiauto.deriveCodec[EnumRestriction])

  implicit val validationProvider: ValidationProvider[EnumRestriction] = ValidationProvider.visitingSequental
}
