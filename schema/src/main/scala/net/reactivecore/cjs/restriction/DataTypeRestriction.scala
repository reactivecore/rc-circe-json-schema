package net.reactivecore.cjs.restriction

import io.circe.generic.semiauto
import io.circe.{Codec, Decoder, Encoder}
import net.reactivecore.cjs.DataTypeName
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{TypesValidator, ValidationProvider}

case class DataTypeRestriction(
    `type`: OValidatingField[DataTypeRestriction.TypeOrTypes, TypesValidator] = None
)

object DataTypeRestriction {
  type TypeOrTypes = Either[DataTypeName, Vector[DataTypeName]]
  implicit val dataTypeVectorCodec: Codec[Vector[DataTypeName]] = Codec.from(
    Decoder.decodeVector[DataTypeName],
    Encoder.encodeVector[DataTypeName]
  )
  implicit val typeCodec: Codec[TypeOrTypes] = Codecs.disjunctEitherCodec[DataTypeName, Vector[DataTypeName]]
  implicit val codec: Codec.AsObject[DataTypeRestriction] = Codecs.withoutNulls(semiauto.deriveCodec[DataTypeRestriction])

  implicit val validationProvider: ValidationProvider[DataTypeRestriction] = ValidationProvider.visitingSequental
}
