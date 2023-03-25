package net.reactivecore.cjs.validator.impl

import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}

/** Typeclass for generating Visiting-Sequential Validation Provider */
trait VisitingSequentialProvider[C] extends ValidationProvider[C]

object VisitingSequentialProvider {
  implicit def provide[C]: VisitingSequentialProvider[C] = ???
}
