package net.reactivecore.cjs.restriction

import io.circe.{Codec, Decoder, Encoder, Json}
import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.{SimpleContextFreeValidator, ValidationProvider, Validator}
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

  /** Context specific validation provider (context may be the outlying class) */
  trait ContextValidationProvider[T, -C] {
    def apply(context: C): ValidationProvider[T]
  }

  /** Provides a validation provider for Validators with trivial constructor */
  implicit def singleValidationProvider[T, V <: Validator](
      implicit generic: Generic.Aux[V, T :: HNil]
  ): ValidationProvider[SingleRestriction[T, V]] = { (origin, value) =>
    generic.from(value.value :: HNil)
  }

  implicit def fromContextProvider[T](implicit p: ValidationProvider[T]): ContextValidationProvider[T, Any] =
    (_: Any) => p

  /** Support for optionals */
  implicit def optionalSupport[T, C](
      implicit underlying: ContextValidationProvider[T, C]
  ): ContextValidationProvider[Option[T], C] = {
    (context: C) =>
      { (origin: SchemaOrigin, restriction: Option[T]) =>
        {
          restriction match {
            case Some(value) => underlying(context)(origin, value)
            case None        => Validator.success
          }
        }
      }
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

  // implicit val trivial = makeTrivialProvider(MinimumValidator.apply)

  case class Example(
      minimum: Option[SingleRestriction[BigDecimal, MinimumValidator]]
  )

  val codec = io.circe.generic.semiauto.deriveCodec[Example]

  // Jetzt fehlt mir nur noch ein ValidationProvider

  trait SequenceValidationProviderBuilder[C] {
    def apply: ValidationProvider[C]
  }

  object SequenceValidationProviderBuilder {

    /** Recursive Helper for Building. */
    trait Helper[T, C] {
      def apply(context: C): ValidationProvider[T]
    }

    implicit def hnilHelper[C]: Helper[HNil, C] = (_: C) => ValidationProvider.empty

    implicit def hlistHelper[H, T <: HList, V <: Validator, C](
        implicit p: ContextValidationProvider[H, C],
        tailHelper: Helper[T, C] // ,
        // w: Witness.Aux[H]
    ): Helper[H :: T, C] =
      new Helper[H :: T, C] {

        override def apply(context: C): ValidationProvider[H :: T] = {
          val fieldName = "TODO"
          ValidationProvider.withOrigin { (origin, value) =>
            Validator.sequence(
              p.apply(context)(origin.enterObject(fieldName), value.head),
              tailHelper.apply(context)(origin, value.tail)
            )
          }
        }
      }

    implicit def generate[C, G](
        implicit labelledGeneric: Generic.Aux[C, G],
        helper: Helper[G, C]
    ): SequenceValidationProviderBuilder[C] = {
      new SequenceValidationProviderBuilder[C] {
        override def apply: ValidationProvider[C] = {
          new ValidationProvider[C] {
            override def apply(origin: SchemaOrigin, restriction: C): Validator = {
              helper.apply(restriction).apply(origin, labelledGeneric.to(restriction))
            }
          }
        }
      }
    }
  }

  def makeSequenceValidationProvider[C](
      implicit builder: SequenceValidationProviderBuilder[C]
  ): ValidationProvider[C] = {
    builder.apply
  }

  implicitly[LabelledGeneric[Example]]

  SequenceValidationProviderBuilder.hnilHelper[Example]

  /*
  SequenceValidationProviderBuilder.hlistHelper[
    Option[SingleRestriction[BigDecimal, MinimumValidator]],
    HNil,
    Example
  ]
   */

  implicitly[
    SequenceValidationProviderBuilder.Helper[Option[SingleRestriction[BigDecimal, MinimumValidator]] :: HNil, Example]
  ]

  makeSequenceValidationProvider[Example]
}
