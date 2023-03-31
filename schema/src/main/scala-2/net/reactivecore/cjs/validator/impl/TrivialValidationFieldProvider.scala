package net.reactivecore.cjs.validator.impl

import net.reactivecore.cjs.validator.Validator
import shapeless._

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
  implicit def trivialValidationProvider[T, V <: Validator](
      implicit generic: Generic.Aux[V, T :: HNil]
  ): TrivialValidationFieldProvider[T, V] = { value =>
    generic.from(value :: HNil)
  }
}
