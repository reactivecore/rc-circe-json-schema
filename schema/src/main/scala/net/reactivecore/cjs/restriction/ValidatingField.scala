package net.reactivecore.cjs.restriction

import io.circe.{Codec, Decoder, Encoder}
import net.reactivecore.cjs.validator.Validator
import net.reactivecore.cjs.validator.provider.ValidationProvider
import shapeless._

/**
  * A Single field inside a Restriction
  *
  * @tparam T type of data field
  * @tparam V tagging type which is used to figure out Validator (usually the Validator itself)
  */
case class ValidatingField[T, V](value: T) extends AnyVal

object ValidatingField {

  /** Provides a validation provider for Validators with trivial constructor */
  implicit def trivialValidationProvider[T, V <: Validator](
      implicit generic: Generic.Aux[V, T :: HNil]
  ): ValidationProvider[ValidatingField[T, V]] = { (origin, value) =>
    generic.from(value.value :: HNil)
  }

  /** JSON Codec Support. */
  implicit def codec[T, V <: Validator](
      implicit e: Encoder[T],
      d: Decoder[T]
  ): Codec[ValidatingField[T, V]] =
    Codec.from(
      d.map(ValidatingField(_)),
      e.contramap(_.value)
    )
}
