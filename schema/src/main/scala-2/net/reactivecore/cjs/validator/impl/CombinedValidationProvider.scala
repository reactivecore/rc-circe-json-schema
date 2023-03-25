package net.reactivecore.cjs.validator.impl

import net.reactivecore.cjs.validator.{ValidationProvider, Validator}
import shapeless._

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
