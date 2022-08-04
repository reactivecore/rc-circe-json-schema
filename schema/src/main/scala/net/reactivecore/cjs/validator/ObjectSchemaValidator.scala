package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}

case class ObjectSchemaValidator(
    origin: SchemaOrigin,
    underlying: Validator,
    override val fragment: Option[String],
    override val dynamicFragment: Option[String],
    override val idOverride: Option[RefUri]
) extends SchemaValidator {
  override def children: Vector[Validator] = Vector(underlying)

  override def touch(state: ValidationState): ValidationState = {
    idOverride match {
      case None =>
        forceTouch(state)
      case Some(_) =>
        // There is a document change
        state
    }
  }

  private def forceTouch(state: ValidationState): ValidationState = {
    val withDynamicAnchor = addDynamicAnchor(state)
    underlying.touch(withDynamicAnchor)
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
        // Already Touched
        underlying.validateStateful(state, json)
      case Some(id) =>
        val pushed = state.pushState(id)
        val beingTouched = forceTouch(pushed)
        val (resultState, violations) = underlying.validateStateful(beingTouched, json)
        (resultState.popState(), violations)
    }
  }
}
