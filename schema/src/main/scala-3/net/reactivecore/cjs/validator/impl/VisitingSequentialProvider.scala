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
    val parts  = provideForTupleWithContext[m.MirroredElemTypes, T]
    val labels = Derivation.deriveLabels[T]
    new VisitingSequentialProvider[T] {
      override def apply(origin: SchemaOrigin, restriction: T): Validator = {
        val validators = restriction.productIterator.zip(parts).zip(labels).map { case ((part, vp), label) =>
          vp.apply(origin.enterObject(label), (part, restriction))
        }
        Validator.sequence(validators.toSeq: _*)
      }
    }
  }

  private[impl] inline def provideForTupleWithContext[T <: Tuple, C]: List[ValidationProvider[(Any, C)]] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val first = summonInline[ValidationProvider[(t, C)]].asInstanceOf[ValidationProvider[(Any, C)]]
        val last = provideForTupleWithContext[ts, C]
        first :: last
    }
  }

}
