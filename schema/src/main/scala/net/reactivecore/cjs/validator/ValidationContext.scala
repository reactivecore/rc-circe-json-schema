package net.reactivecore.cjs.validator

import net.reactivecore.cjs.resolver.RefUri

/** Referenced context between validators. */
trait ValidationContext {

  /**
    * Resolves a validator for a reference.
    *
    * @return either an error, or the validator and an optionally updated validation context
    */
  def resolve(refUri: RefUri): Either[String, Validator]

  /** Resolve a validator for a dynamic reference. */
  def resolveDynamic(state: ValidationState, refUri: RefUri): Either[String, Validator]
}
