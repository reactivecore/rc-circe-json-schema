package net.reactivecore.cjs.restriction

import io.circe.{Codec, Decoder, Encoder, Json}
import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.{SimpleContextFreeValidator, ValidationProvider, Validator}
import shapeless._

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

  trait SingleRestrictionProvider[-C, T, V] {
    def apply(context: C, origin: SchemaOrigin, value: T): Validator
  }

  // Manuelle Version um Provider zu bauen
  def makeTrivialProvider[T, V](f: T => Validator): SingleRestrictionProvider[Any, T, V] =
    new SingleRestrictionProvider[Any, T, V] {
      override def apply(context: Any, origin: SchemaOrigin, value: T): Validator = f(value)
    }

  // Implicite Variante um Provider zu bauen
  implicit def provideTrivial[T, V <: Validator](
      implicit generic: Generic.Aux[V, T :: HNil]
  ): SingleRestrictionProvider[Any, T, V] = new SingleRestrictionProvider[Any, T, V] {
    override def apply(context: Any, origin: SchemaOrigin, value: T): Validator = {
      generic.from(value :: HNil)
    }
  }

  implicit def provideOption[C, T, V <: Validator](
      implicit underlying: SingleRestrictionProvider[C, T, V]
  ): SingleRestrictionProvider[C, Option[T], V] = new SingleRestrictionProvider[C, Option[T], V] {
    override def apply(context: C, origin: SchemaOrigin, value: Option[T]): Validator = {
      value match {
        case Some(value) => underlying.apply(context, origin, value)
        case None        => Validator.success
      }
    }
  }

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

  implicitly[SingleRestrictionProvider[Example, BigDecimal, MinimumValidator]]

  // Das geht auch
  implicitly[SingleRestrictionProvider[Example, Option[BigDecimal], MinimumValidator]]

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

    implicit def hnilHelper[C]: Helper[HNil, C] = new Helper[HNil, C] {
      override def apply(context: C): ValidationProvider[HNil] = ValidationProvider.empty
    }

    implicit def hlistHelper[H, T <: HList, V <: Validator, C](
        implicit p: SingleRestrictionProvider[C, H, V],
        tailHelper: Helper[T, C] // ,
        // w: Witness.Aux[H]
    ): Helper[SingleRestriction[H, V] :: T, C] =
      new Helper[SingleRestriction[H, V] :: T, C] {

        override def apply(context: C): ValidationProvider[SingleRestriction[H, V] :: T] = {
          val fieldName = "TODO"
          ValidationProvider.withOrigin { (origin, value) =>
            Validator.sequence(
              p.apply(context, origin.enterObject(fieldName), value.head.value),
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

  SequenceValidationProviderBuilder.hlistHelper[
    Option[SingleRestriction[BigDecimal, MinimumValidator]],
    HNil,
    MinimumValidator,
    Example
  ]

  implicitly[
    SequenceValidationProviderBuilder.Helper[Option[SingleRestriction[BigDecimal, MinimumValidator]] :: HNil, Example]
  ]
  // SequenceValidationProviderBuilder.generate[Example, Option[SingleRestriction[BigDecimal, MinimumValidator]] :: HNil]

  makeSequenceValidationProvider[Example] // Doesn't compile yet
}
