package net.reactivecore.cjs.validator.provider

import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.Validator
import shapeless._

/** Typeclass which provides Validators for Restrictions */
trait ValidationProvider[T] {
  def apply(origin: SchemaOrigin, restriction: T): Validator
}

object ValidationProvider {

  def instance[T](instance: T => Validator): ValidationProvider[T] = { (_, restriction: T) =>
    instance(restriction)
  }

  def withOrigin[T](instance: (SchemaOrigin, T) => Validator): ValidationProvider[T] = { (origin, restriction: T) =>
    instance(origin, restriction)
  }

  def empty[T]: ValidationProvider[T] = ValidationProvider.instance(_ => Validator.success)

  /** Helper trait for synthetic validation provider for case classes */
  trait CombinedValidationProvider[T] extends ValidationProvider[T]

  object CombinedValidationProvider {
    implicit val nil: CombinedValidationProvider[HNil] = (_, _: HNil) => Validator.success

    implicit def elem[H, T <: HList](
        implicit hc: ValidationProvider[H],
        tc: CombinedValidationProvider[T]
    ): CombinedValidationProvider[H :: T] =
      (context, restriction: H :: T) => {
        Validator.sequence(
          Seq(
            hc(context, restriction.head),
            tc(context, restriction.tail)
          ): _*
        )
      }

    implicit def generic[T, G](
        implicit g: Generic.Aux[T, G],
        p: CombinedValidationProvider[G]
    ): CombinedValidationProvider[T] = { (context, restriction: T) =>
      p.apply(context, g.to(restriction))
    }
  }

  def combineG[T](implicit combinedValidationProvider: CombinedValidationProvider[T]): ValidationProvider[T] = {
    combinedValidationProvider
  }
}
