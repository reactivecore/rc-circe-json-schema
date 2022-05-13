package net.reactivecore.cjs.restriction

import io.circe.generic.semiauto
import io.circe.{Codec, Decoder, Encoder}
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{TypesValidator, Validator}
import net.reactivecore.cjs.DataTypeName
import net.reactivecore.cjs.validator.provider.ValidationProvider

case class DataTypeRestriction(
    `type`: Option[DataTypeRestriction.TypeOrTypes] = None
) {
  def types: Option[Vector[DataTypeName]] = {
    `type`.map {
      case Left(single) => Vector(single)
      case Right(many)  => many
    }
  }
}

object DataTypeRestriction {
  type TypeOrTypes = Either[DataTypeName, Vector[DataTypeName]]
  implicit val dataTypeVectorCodec: Codec[Vector[DataTypeName]] = Codec.from(
    Decoder.decodeVector[DataTypeName],
    Encoder.encodeVector[DataTypeName]
  )
  implicit val typeCodec: Codec[TypeOrTypes] = Codecs.disjunctEitherCodec[DataTypeName, Vector[DataTypeName]]
  implicit val codec: Codec.AsObject[DataTypeRestriction] = Codecs.withoutNulls(semiauto.deriveCodec)

  implicit val validationProvider: ValidationProvider[DataTypeRestriction] = ValidationProvider.instance {
    restriction =>
      Validator.sequence(
        restriction.types match {
          case None => Validator.success
          case Some(types) =>
            TypesValidator(types)
        }
      )
  }
}
