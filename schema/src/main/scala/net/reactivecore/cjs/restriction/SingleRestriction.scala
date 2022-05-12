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

  trait SingleRestrictionProvider[-C, T, V <: Validator] {
    def apply(context: C, origin: SchemaOrigin, value: T): V
  }

  // Manuelle Version um Provider zu bauen
  def makeTrivialProvider[T, V <: Validator](f: T => V): SingleRestrictionProvider[Any, T, V] =
    new SingleRestrictionProvider[Any, T, V] {
      override def apply(context: Any, origin: SchemaOrigin, value: T): V = f(value)
    }

  // Implicite Variante um Provider zu bauen
  implicit def provideTrivial[T, V <: Validator](
      implicit generic: Generic.Aux[V, T :: HNil]
  ): SingleRestrictionProvider[Any, T, V] = new SingleRestrictionProvider[Any, T, V] {
    override def apply(context: Any, origin: SchemaOrigin, value: T): V = {
      generic.from(value :: HNil)
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

  val codec = io.circe.generic.semiauto.deriveCodec[Example]

  // Jetzt fehlt mir nur noch ein ValidationProvider

  trait Helper[T, C] {
    def apply(context: C): ValidationProvider[T]
  }

  implicit def hnilHelper[C]: Helper[HNil, C] = new Helper[HNil, C] {
    override def apply(context: C): ValidationProvider[HNil] = ValidationProvider.empty
  }

  implicit def hlistHelper[H, T <: HList, V <: Validator, C](
      implicit p: SingleRestrictionProvider[C, H, V],
      tailHelper: Helper[T, C],
      w: Witness.Aux[H]
  ) =
    new Helper[H :: T, C] {
      override def apply(context: C): ValidationProvider[H :: T] = {
        val fieldName = "TODO"
        ValidationProvider.withOrigin { (origin, value) =>
          Validator.sequence(
            p.apply(context, origin.enterObject(fieldName), value.head),
            tailHelper.apply(context)(origin, value.tail)
          )
        }
      }
    }

  def makeValidationProvider[T, G](
      implicit labelledGeneric: LabelledGeneric.Aux[T, G],
      helper: Helper[G, T]
  ): ValidationProvider[T] = {
    new ValidationProvider[T] {
      override def apply(origin: SchemaOrigin, restriction: T): Validator = {
        helper.apply(restriction).apply(origin, labelledGeneric.to(restriction))
      }
    }
  }

  // TODO: Continue here!

  // makeValidationProvider[Example] // Doesn't compile yet
}
