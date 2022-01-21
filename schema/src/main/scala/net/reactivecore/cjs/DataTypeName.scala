package net.reactivecore.cjs

import io.circe.{Codec, Decoder, Encoder, Json}

sealed abstract class DataTypeName(val name: String)

object DataTypeName {
  case object DtString extends DataTypeName("string")
  case object DtArray extends DataTypeName("array")
  case object DtObject extends DataTypeName("object")
  case object DtBoolean extends DataTypeName("boolean")
  case object DtNumber extends DataTypeName("number")
  case object DtNull extends DataTypeName("null")
  case object DtInteger extends DataTypeName("integer")

  val all = Seq(DtString, DtArray, DtObject, DtBoolean, DtNumber, DtNull, DtInteger)

  implicit val encoder: Encoder[DataTypeName] = Encoder.encodeString.contramap(_.name)
  implicit val decoder: Decoder[DataTypeName] = Decoder.decodeString.emap { name =>
    all.find(_.name == name) match {
      case None        => Left(s"Type with name ${name} not found")
      case Some(value) => Right(value)
    }
  }
  implicit val codec: Codec[DataTypeName] = Codec.from(decoder, encoder)
}
