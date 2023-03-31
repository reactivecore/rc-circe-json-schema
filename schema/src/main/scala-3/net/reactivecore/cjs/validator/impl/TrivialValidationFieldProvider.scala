package net.reactivecore.cjs.validator.impl

import net.reactivecore.cjs.validator.Validator
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}


/** Helper type class which is present when a validator V can be instantiated on
  * one single value T
  */
trait TrivialValidationFieldProvider[T, V <: Validator] {
  def provide(value: T): V
}

object TrivialValidationFieldProvider {

  /** Generate from  it, if the Validator class has only a single field.
    * (Ok, that's a bit hacky)
    */
  implicit inline def trivialValidationProvider[T, V <: Validator](using m: Mirror.ProductOf[V]): TrivialValidationFieldProvider[T, V] = {
    val b = singleBuilder[T, m.MirroredElemTypes]
    new TrivialValidationFieldProvider[T, V] {
        def provide(value: T): V = {
          m.fromTuple(b(value))
        }
    }
  }



  private inline def singleBuilder[T, V <: Tuple]: T => V = {
    inline erasedValue[V] match {
      case _: (T *: EmptyTuple) =>
        value => (value *: EmptyTuple).asInstanceOf[V]
    }
  }
}
