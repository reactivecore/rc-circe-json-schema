package net.reactivecore.cjs.restriction

import io.circe.{Codec, Decoder, Encoder}
import net.reactivecore.cjs.validator.impl.TrivialValidationFieldProvider
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}

/**
  * A Single field inside a Restriction
  *
  * @tparam T type of data field
  * @tparam V tagging type which is used to figure out Validator (usually the Validator itself)
  */
case class ValidatingField[T, V](value: T) extends AnyVal

object ValidatingField {

  implicit def trivial[T, V <: Validator](
      implicit t: TrivialValidationFieldProvider[T, V]
  ): ValidationProvider[ValidatingField[T, V]] = { (_, value) =>
    t.provide(value.value)
  }

  /** Empty validation providers for fields which do not have one. */
  implicit def successValidationProvider[T]: ValidationProvider[ValidatingField[T, Validator.Success.type]] = {
    (_, _) =>
      Validator.success
  }

  /** JSON Codec Support. */
  implicit def codec[T, V](
      implicit e: Encoder[T],
      d: Decoder[T]
  ): Codec[ValidatingField[T, V]] =
    Codec.from(
      d.map(ValidatingField(_)),
      e.contramap(_.value)
    )
}
