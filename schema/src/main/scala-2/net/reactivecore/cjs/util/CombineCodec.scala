package net.reactivecore.cjs.util

import io.circe.Decoder.Result
import io.circe.{Codec, HCursor, JsonObject}
import shapeless._

trait CombineCodec[T] {
  def codec: Codec.AsObject[T]
}

object CombineCodec {
  implicit val nilCodec: CombineCodec[HNil] = new CombineCodec[HNil] {
    override def codec: Codec.AsObject[HNil] = new Codec.AsObject[HNil] {
      override def apply(c: HCursor): Result[HNil] = Right(HNil)

      override def encodeObject(a: HNil): JsonObject = JsonObject()
    }
  }

  implicit def elemCodec[H, T <: HList](
      implicit hc: Codec.AsObject[H],
      tc: CombineCodec[T]
  ): CombineCodec[H :: T] =
    new CombineCodec[H :: T] {
      override def codec: Codec.AsObject[H :: T] = Codec.AsObject.from[H :: T](
        Codecs.combineDecoderA[H :: T, H, T]((a, b) => a :: b)(hc, tc.codec),
        Codecs.combineEncoderU[H :: T, H, T](c => Some(c.head, c.tail))(hc, tc.codec)
      )
    }

  implicit def genericCodec[T, G](implicit g: Generic.Aux[T, G], c: CombineCodec[G]): CombineCodec[T] = {
    new CombineCodec[T] {
      override def codec: Codec.AsObject[T] = Codec.AsObject.from(
        c.codec.map(g.from),
        c.codec.contramapObject(g.to)
      )
    }
  }
}
