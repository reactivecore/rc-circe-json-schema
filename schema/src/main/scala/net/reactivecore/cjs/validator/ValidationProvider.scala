package net.reactivecore.cjs.validator

import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.restriction.ValidatingField
import shapeless._
import shapeless.labelled.FieldType

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

  /**
    * Generates a simple sequence validation provider for generic types.
    * (all types are evaluated without entering an object)
    */
  def combined[T](implicit combinedValidationProvider: CombinedValidationProvider[T]): ValidationProvider[T] = {
    combinedValidationProvider
  }

  /**
    * Helper for generating [[visitingSequental]]
    * @tparam T LabelledGeneric instance.
    * @tparam C context class (enclosing class)
    */
  trait VisitingSequentialHelper[T, C] {
    def apply(context: C): ValidationProvider[T]
  }

  object VisitingSequentialHelper {
    implicit def hnilHelper[C]: VisitingSequentialHelper[HNil, C] = (_: C) => ValidationProvider.empty

    implicit def hlistHelper[K <: Symbol, H, T <: HList, V <: Validator, C](
        implicit p: ValidationProvider[H],
        tailHelper: VisitingSequentialHelper[T, C],
        w: Witness.Aux[K]
    ): VisitingSequentialHelper[FieldType[K, H] :: T, C] =
      new VisitingSequentialHelper[FieldType[K, H] :: T, C] {
        val fieldName = w.value.name

        override def apply(context: C): ValidationProvider[FieldType[K, H] :: T] = {
          ValidationProvider.withOrigin { (origin, value) =>
            Validator.sequence(
              p(origin.enterObject(fieldName), value.head),
              tailHelper.apply(context)(origin, value.tail)
            )
          }
        }
      }

    implicit def hlistHelperWithContext[K <: Symbol, H, T <: HList, V <: Validator, C](
        implicit p: ValidationProvider[(H, C)],
        tailHelper: VisitingSequentialHelper[T, C],
        w: Witness.Aux[K]
    ): VisitingSequentialHelper[FieldType[K, H] :: T, C] =
      new VisitingSequentialHelper[FieldType[K, H] :: T, C] {
        val fieldName = w.value.name

        override def apply(context: C): ValidationProvider[FieldType[K, H] :: T] = {
          ValidationProvider.withOrigin { (origin, value) =>
            Validator.sequence(
              p(origin.enterObject(fieldName), value.head -> context),
              tailHelper.apply(context)(origin, value.tail)
            )
          }
        }
      }
  }

  /** Typeclass for generating [[visitingSequental]] */
  trait VisitingSequentialProvider[C] {
    def apply: ValidationProvider[C]
  }

  object VisitingSequentialProvider {
    implicit def generate[C, G](
        implicit labelledGeneric: LabelledGeneric.Aux[C, G],
        helper: VisitingSequentialHelper[G, C]
    ): VisitingSequentialProvider[C] = {
      new VisitingSequentialProvider[C] {
        override def apply: ValidationProvider[C] = { (origin: SchemaOrigin, restriction: C) =>
          {
            helper.apply(restriction).apply(origin, labelledGeneric.to(restriction))
          }
        }
      }
    }
  }

  /**
    * Generate a validation provider for a case class building up from perhaps context-aware Validation Providers.
    * Each sub field's name will be visited (and can be disabled when having a vocabulary)
    */
  def visitingSequental[T](implicit p: VisitingSequentialProvider[T]): ValidationProvider[T] = p.apply

}
