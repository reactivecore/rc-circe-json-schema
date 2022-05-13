package net.reactivecore.cjs.validator.provider

import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.Validator

/**
  * Typeclass which provides Validators but need some context.
  * The context is usually the surrounding Case class where the data resides.
  *
  * @tparam T the data which is used to generate the Validator
  * @tparam C the context (usually the surrounding class of T)
  */
trait ContextValidationProvider[T, -C] {
  def apply(context: C): ValidationProvider[T]
}

object ContextValidationProvider {

  /** Support if there is a direct validation provider. */
  implicit def fromValidationProvider[T](implicit p: ValidationProvider[T]): ContextValidationProvider[T, Any] =
    (_: Any) => p

  /** Support for optionals (empty data leads to successful validation) */
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
}
