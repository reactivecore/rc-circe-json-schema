package net.reactivecore.cjs.util

import io.circe.Decoder.Result
import io.circe._

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
