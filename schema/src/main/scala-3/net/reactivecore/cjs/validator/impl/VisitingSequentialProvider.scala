package net.reactivecore.cjs.validator.impl

import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import net.reactivecore.cjs.util.Derivation


/** Typeclass for generating Visiting-Sequential Validation Provider */
trait VisitingSequentialProvider[C] extends ValidationProvider[C]

object VisitingSequentialProvider {
  implicit inline def provide[T <: Product](using m: Mirror.ProductOf[T]): VisitingSequentialProvider[T] = {
    val parts  = CombinedValidationProvider.provideForTuple[m.MirroredElemTypes]
    val labels = Derivation.deriveLabels[T]
    new VisitingSequentialProvider[T] {
      override def apply(origin: SchemaOrigin, restriction: T): Validator = {
        val validators = restriction.productIterator.zip(parts).zip(labels).map { case ((part, vp), label) =>
          vp.apply(origin.enterObject(label), part)
        }
        Validator.sequence(validators.toSeq: _*)
      }
    }
  }
}
