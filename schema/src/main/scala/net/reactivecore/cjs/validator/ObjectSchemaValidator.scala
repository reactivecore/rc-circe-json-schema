package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}

case class ObjectSchemaValidator(
    path: JsonPointer,
    underlying: Validator,
    override val fragment: Option[String],
    override val dynamicFragment: Option[String],
    idOverride: Option[RefUri]
) extends SchemaValidator {
  override def children: Vector[Validator] = Vector(underlying)

  override def touch(state: ValidationState): ValidationState = {
    idOverride match {
      case None =>
        underlying.touch(addDynamicAnchor(state))
      case Some(id) =>
        // It would be popped anyway
        state
    }
  }

  private def addDynamicAnchor(state: ValidationState): ValidationState = {
    dynamicFragment match {
      case Some(existing) =>
        state.withDynamicAnchor(existing, this)
      case _ =>
        state
    }
  }

  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    idOverride match {
      case None =>
        val withAnchor = addDynamicAnchor(state)
        underlying.validateStateful(withAnchor, json)
      case Some(id) =>
        val pushed = addDynamicAnchor(state.pushState(id))
        val (resultState, violations) = underlying.validateStateful(pushed, json)
        (resultState.popState(), violations)
    }
  }
}
