package net.reactivecore.cjs.restriction

import io.circe.{Codec, Decoder, Encoder}
import net.reactivecore.cjs.validator.Validator
import net.reactivecore.cjs.validator.provider.ValidationProvider
import shapeless._

/** Holds a single named restriction */
case class SingleRestriction[T, VF](value: T) extends AnyVal

object SingleRestriction {

  /** Provides a validation provider for Validators with trivial constructor */
  implicit def singleValidationProvider[T, V <: Validator](
      implicit generic: Generic.Aux[V, T :: HNil]
  ): ValidationProvider[SingleRestriction[T, V]] = { (origin, value) =>
    generic.from(value.value :: HNil)
  }

  /** JSON Codec Support. */
  implicit def singleRestrictionCodec[T, V <: Validator](
      implicit e: Encoder[T],
      d: Decoder[T]
  ): Codec[SingleRestriction[T, V]] =
    Codec.from(
      d.map(SingleRestriction(_)),
      e.contramap(_.value)
    )
}
