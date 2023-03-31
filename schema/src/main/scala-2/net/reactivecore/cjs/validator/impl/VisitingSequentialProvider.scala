package net.reactivecore.cjs.validator.impl

import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}
import shapeless._
import shapeless.labelled.FieldType

/** Typeclass for generating Visiting-Sequential Validation Provider */
trait VisitingSequentialProvider[C] extends ValidationProvider[C]

object VisitingSequentialProvider {
  implicit def generate[C, G](
      implicit labelledGeneric: LabelledGeneric.Aux[C, G],
      helper: VisitingSequentialHelper[G, C]
  ): VisitingSequentialProvider[C] = { (origin: SchemaOrigin, restriction: C) =>
    {
      helper.apply(restriction).apply(origin, labelledGeneric.to(restriction))
    }
  }

  /**
    * Helper for generating [[visitingSequental]]
    *
    * @tparam T LabelledGeneric instance.
    * @tparam C context class (enclosing class)
    */
  trait VisitingSequentialHelper[T, C] {
    def apply(context: C): ValidationProvider[T]
  }

  object VisitingSequentialHelper {
    implicit def hnilHelper[C]: VisitingSequentialHelper[HNil, C] = (_: C) => ValidationProvider.empty

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
}
