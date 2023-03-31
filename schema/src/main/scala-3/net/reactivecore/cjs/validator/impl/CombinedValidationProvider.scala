package net.reactivecore.cjs.validator.impl

import net.reactivecore.cjs.validator.{ValidationProvider, Validator}
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}
import net.reactivecore.cjs.SchemaOrigin

/** Helper trait for synthetic validation provider for case classes */
trait CombinedValidationProvider[T] extends ValidationProvider[T]

object CombinedValidationProvider {
  implicit inline def provide[T <: Product](using m: Mirror.ProductOf[T]): CombinedValidationProvider[T] = {
    val tupleVariant  = provideForTuple[m.MirroredElemTypes]
    new CombinedValidationProvider[T] {
      override def apply(origin: SchemaOrigin, restriction: T): Validator = {
        val validators = restriction.productIterator.zip(tupleVariant).map { case (field, subValidationProvider) =>
          subValidationProvider(origin, field)
        }
        Validator.sequence(validators.toSeq: _*)
      }
    }
  }

  private[impl] inline def provideForTuple[T <: Tuple]: List[ValidationProvider[Any]] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        val first = summonInline[ValidationProvider[t]].asInstanceOf[ValidationProvider[Any]]
        val last = provideForTuple[ts]
        first :: last
    }
  }
}