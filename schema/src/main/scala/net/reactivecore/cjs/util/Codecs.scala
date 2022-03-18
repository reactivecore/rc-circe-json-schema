package net.reactivecore.cjs.util

import io.circe.Decoder.Result
import io.circe.{Codec, Decoder, Encoder, HCursor, Json, JsonObject}
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

/** JSON Codec Helpers */
object Codecs {

  /** Combine two encoders into one */
  def combineEncoder[A, B](implicit ae: Encoder.AsObject[A], be: Encoder.AsObject[B]): Encoder.AsObject[(A, B)] = {
    Encoder.AsObject { case (a, b) =>
      JsonObject.fromIterable(
        ae.encodeObject(a).toIterable ++ be.encodeObject(b).toIterable
      )
    }
  }

  /** Combine two encoders into one with some unapply method from T's companion */
  def combineEncoderU[T, A, B](
      unapply: T => Option[(A, B)]
  )(implicit ae: Encoder.AsObject[A], be: Encoder.AsObject[B]): Encoder.AsObject[T] = {
    combineEncoder[A, B].contramapObject[T] { value =>
      unapply(value).get
    }
  }

  /** Combine two decoders into one. */
  def combineDecoder[A, B](implicit ad: Decoder[A], bd: Decoder[B]): Decoder[(A, B)] = {
    Decoder { json =>
      for {
        a <- ad(json)
        b <- bd(json)
      } yield (a, b)
    }
  }

  /** Combine two decoders into one with apply-Method for constructing a case class */
  def combineDecoderA[T, A, B](apply: (A, B) => T)(implicit ad: Decoder[A], bd: Decoder[B]): Decoder[T] = {
    combineDecoder[A, B].map { case (a, b) => apply(a, b) }
  }

  def combineCodecG[T](implicit combineCodec: CombineCodec[T]): Codec.AsObject[T] = {
    combineCodec.codec
  }

  /** Drop null values inside an object encoder */
  def withoutNulls[T](encoder: Encoder.AsObject[T]): Encoder.AsObject[T] = {
    encoder.mapJsonObject(_.filter { case (_, v) => !v.isNull })
  }

  def withoutNulls[T](codec: Codec.AsObject[T]): Codec.AsObject[T] = {
    Codec.AsObject.from(codec, withoutNulls(codec: Encoder.AsObject[T]))
  }

  def disjunctEitherCodec[L, R](
      implicit leftE: Encoder[L],
      leftD: Decoder[L],
      rightE: Encoder[R],
      rightD: Decoder[R]
  ): Codec[Either[L, R]] = {
    new Codec[Either[L, R]] {
      override def apply(c: HCursor): Result[Either[L, R]] = {
        leftD(c) match {
          case Right(ok) => Right(Left(ok))
          case Left(_) =>
            rightD(c).map(Right(_))
        }
      }

      override def apply(a: Either[L, R]): Json = {
        a match {
          case Left(l)  => leftE(l)
          case Right(r) => rightE(r)
        }
      }
    }
  }
}
