package net.reactivecore.cjs.validator

import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.restriction.ValidatingField
import net.reactivecore.cjs.validator.impl.{CombinedValidationProvider, VisitingSequentialProvider}

/** Typeclass which provides Validators for Restrictions.
  * @tparam T the data from which a validator is created. Some validation providers can automatically be deduced from
  *          ValidatingFields.
  */
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

  def forField[T, V](f: (SchemaOrigin, T) => Validator): ValidationProvider[ValidatingField[T, V]] = {
    withOrigin[ValidatingField[T, V]] { (origin, field) =>
      f(origin, field.value)
    }
  }

  def forFieldWithContext[T, V, C](
      f: (SchemaOrigin, T, C) => Validator
  ): ValidationProvider[(ValidatingField[T, V], C)] = {
    withOrigin[(ValidatingField[T, V], C)] { case (origin, (field, context)) =>
      f(origin, field.value, context)
    }
  }

  def empty[T]: ValidationProvider[T] = ValidationProvider.instance(_ => Validator.success)

  implicit def forOptionalField[T, V](
      implicit fieldProvider: ValidationProvider[ValidatingField[T, V]]
  ): ValidationProvider[Option[ValidatingField[T, V]]] = { (origin, value) =>
    value.map(fieldProvider(origin, _)).getOrElse(Validator.success)
  }

  implicit def forOptionalFieldWithContext[T, V, C](
      implicit fieldProvider: ValidationProvider[(ValidatingField[T, V], C)]
  ): ValidationProvider[(Option[ValidatingField[T, V]], C)] = { (origin, value) =>
    value._1
      .map { v1 =>
        fieldProvider(origin, (v1, value._2))
      }
      .getOrElse(Validator.success)
  }

  /** Automatically provides a context aware ValidationProvider, if there is one, which doesn't need it */
  implicit def withContext[T, C](implicit v: ValidationProvider[T]): ValidationProvider[(T, C)] = { (origin, value) =>
    v(origin, value._1)
  }


  /**
    * Generates a simple sequence validation provider for generic types.
    * (all types are evaluated without entering an object)
    */
  def combined[T](implicit combinedValidationProvider: CombinedValidationProvider[T]): ValidationProvider[T] = {
    combinedValidationProvider
  }

  /**
    * Generate a validation provider for a case class building up from perhaps context-aware Validation Providers.
    * Each sub field's name will be visited
    */
  def visitingSequental[T](implicit p: VisitingSequentialProvider[T]): ValidationProvider[T] = p
}
