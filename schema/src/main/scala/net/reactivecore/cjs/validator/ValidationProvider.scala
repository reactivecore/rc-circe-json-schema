package net.reactivecore.cjs.validator

import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}
import shapeless._

/** Typeclass which provides Validators for Restrictions */
trait ValidationProvider[T] {
  def apply(parentId: RefUri, path: JsonPointer, restriction: T): Validator
}

object ValidationProvider {

  def instance[T](instance: T => Validator): ValidationProvider[T] = { (_, _, restriction: T) =>
    instance(restriction)
  }

  def withUri[T](instance: (RefUri, JsonPointer, T) => Validator): ValidationProvider[T] = { (uri, path, restriction) =>
    instance(uri, path, restriction)
  }

  def empty[T]: ValidationProvider[T] = ValidationProvider.instance(_ => Validator.success)

  /** Helper trait for synthetic validation provider for case classes */
  trait CombinedValidationProvider[T] extends ValidationProvider[T]

  object CombinedValidationProvider {
    implicit val nil: CombinedValidationProvider[HNil] = (_, _, _: HNil) => Validator.success

    implicit def elem[H, T <: HList](
        implicit hc: ValidationProvider[H],
        tc: CombinedValidationProvider[T]
    ): CombinedValidationProvider[H :: T] =
      (parentId, path, restriction: H :: T) => {
        Validator.sequence(
          Seq(
            hc(parentId, path, restriction.head),
            tc(parentId, path, restriction.tail)
          ): _*
        )
      }

    implicit def generic[T, G](
        implicit g: Generic.Aux[T, G],
        p: CombinedValidationProvider[G]
    ): CombinedValidationProvider[T] = { (parentId, path, restriction: T) =>
      p.apply(parentId, path, g.to(restriction))
    }
  }

  def combineG[T](implicit combinedValidationProvider: CombinedValidationProvider[T]): ValidationProvider[T] = {
    combinedValidationProvider
  }
}
