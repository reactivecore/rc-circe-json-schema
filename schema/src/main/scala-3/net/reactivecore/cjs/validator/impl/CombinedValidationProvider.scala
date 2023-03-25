package net.reactivecore.cjs.validator.impl

import net.reactivecore.cjs.validator.{ValidationProvider, Validator}

/** Helper trait for synthetic validation provider for case classes */
trait CombinedValidationProvider[T] extends ValidationProvider[T]

object CombinedValidationProvider {
  implicit def provide[T]: CombinedValidationProvider[T] = ???
}
