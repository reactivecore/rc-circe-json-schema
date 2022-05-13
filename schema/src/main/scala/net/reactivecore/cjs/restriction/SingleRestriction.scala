package net.reactivecore.cjs.restriction

import io.circe.{Codec, Decoder, Encoder, Json}
import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.provider.{
  ContextValidationProvider,
  SequenceValidationProvider,
  ValidationProvider
}
import net.reactivecore.cjs.validator.{SimpleContextFreeValidator, Validator}
import shapeless._
import shapeless.labelled.FieldType

/** Holds a single named restriction */
case class SingleRestriction[T, VF](value: T) extends AnyVal

object SingleRestriction {
  case class MinimumValidator(value: BigDecimal) extends SimpleContextFreeValidator("minimum") {
    override def isValid(json: Json): Boolean = {
      json.as[BigDecimal] match {
        case Left(_)                => true
        case Right(c) if c >= value => true
        case _                      => false
      }
    }
  }

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

  // Ok, das geht schonmal
  singleRestrictionCodec[BigDecimal, MinimumValidator]

  // Das geht auch
  implicitly[Codec[SingleRestriction[BigDecimal, MinimumValidator]]]

  case class Example(
      minimum: Option[SingleRestriction[BigDecimal, MinimumValidator]]
  )

  val codec = io.circe.generic.semiauto.deriveCodec[Example]

  implicitly[ValidationProvider[SingleRestriction[BigDecimal, MinimumValidator]]]
  implicitly[ContextValidationProvider[SingleRestriction[BigDecimal, MinimumValidator], Example]]

  // Jetzt fehlt mir nur noch ein ValidationProvider

  val lg = LabelledGeneric[Example]
  val helper = implicitly[SequenceValidationProvider.Helper[lg.Repr, Example]]

  SequenceValidationProvider.apply[Example]
}
