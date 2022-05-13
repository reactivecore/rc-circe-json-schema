package net.reactivecore.cjs.validator.provider

import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.Validator
import shapeless._
import shapeless.labelled.FieldType

/**
  * Type class for building a ValidationProvider for a case class where all fields
  *  can build [[ContextValidationProvider]]
  */
trait SequenceValidationProvider[C] {
  def apply: ValidationProvider[C]
}

/**
  * Generates a ValidationProvider for a case class by comining all fields into one ValidationProvider using
  * Validator.sequence.
  *
  * The field names are respected.
  */
object SequenceValidationProvider {

  /** Recursive Helper for Building. */
  trait Helper[T, C] {
    def apply(context: C): ValidationProvider[T]
  }

  object Helper {
    implicit def hnilHelper[C]: Helper[HNil, C] = (_: C) => ValidationProvider.empty

    implicit def hlistHelper[K <: Symbol, H, T <: HList, V <: Validator, C](
        implicit p: ContextValidationProvider[H, C],
        tailHelper: Helper[T, C],
        w: Witness.Aux[K]
    ): Helper[FieldType[K, H] :: T, C] =
      new Helper[FieldType[K, H] :: T, C] {
        val fieldName = w.value.name

        override def apply(context: C): ValidationProvider[FieldType[K, H] :: T] = {
          ValidationProvider.withOrigin { (origin, value) =>
            Validator.sequence(
              p.apply(context)(origin.enterObject(fieldName), value.head),
              tailHelper.apply(context)(origin, value.tail)
            )
          }
        }
      }
  }

  implicit def generate[C, G](
      implicit labelledGeneric: LabelledGeneric.Aux[C, G],
      helper: Helper[G, C]
  ): SequenceValidationProvider[C] = {
    new SequenceValidationProvider[C] {
      override def apply: ValidationProvider[C] = { (origin: SchemaOrigin, restriction: C) =>
        {
          helper.apply(restriction).apply(origin, labelledGeneric.to(restriction))
        }
      }
    }
  }

  /** Generate a ValidationProvider for case class C */
  def apply[C](implicit s: SequenceValidationProvider[C]): ValidationProvider[C] = s.apply
}
