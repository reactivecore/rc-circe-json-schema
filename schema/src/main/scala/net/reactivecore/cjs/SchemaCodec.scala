package net.reactivecore.cjs

import io.circe.{Codec, Decoder, Encoder}
import net.reactivecore.cjs.util.Codecs

private[cjs] object SchemaCodec {
  val booleanSchemaCodec: Codec[BooleanSchema] = Codec.from(
    Decoder.decodeBoolean.map(BooleanSchema.apply),
    Encoder.encodeBoolean.contramap(x => x.value)
  )

  lazy val schemaCodec: Codec[Schema] = {
    val base = Codecs.disjunctEitherCodec[BooleanSchema, ObjectSchema]
    Codec.from(
      base.map {
        case Left(left)   => left
        case Right(value) => value
      },
      base.contramap {
        case b: BooleanSchema => Left(b)
        case r: ObjectSchema  => Right(r)
      }
    )
  }
}
