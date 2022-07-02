package net.reactivecore.cjs.vocabulary

import io.circe.Json
import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}
import net.reactivecore.cjs.{BooleanSchema, Description, ObjectSchema, Schema, SchemaOrigin}
import net.reactivecore.cjs.validator.{
  BooleanSchemaValidator,
  SchemaValidator,
  ValidationContext,
  ValidationResult,
  ValidationState,
  Validator
}

/** Root of a schema */
sealed trait Schema {

  /** Returns a validator for the schema. */
  def validator(origin: SchemaOrigin): SchemaValidator

  /**
    * Returns a [[SchemaValidator]] for this Schema.
    *
    * In contrast to [[validator]], the URI is already given, and the root $id will be ignored.
    */
  def schemaValidator(id: RefUri): SchemaValidator = {
    // TODO: ID Override
    validator(SchemaOrigin(id, JsonPointer(), None))
  }
}

/** A Schema which directly evaluates into true or false */
case class BooleanSchema(value: Boolean) extends Schema {
  override def validator(origin: SchemaOrigin): SchemaValidator = {
    BooleanSchemaValidator(origin, value)
  }
}

case class NewSchemaValidator(origin: SchemaOrigin, mainValidator: Validator) extends SchemaValidator {
  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    mainValidator.validateStateful(state, json)
  }
}

case class ObjectSchema(
    entities: Vector[VocabularyEntity]
) extends Schema {
  override def validator(origin: SchemaOrigin): SchemaValidator = {
    val subValidators = entities.map(_.validator(origin))
    NewSchemaValidator(
      origin,
      Validator.sequence(subValidators: _*)
    )
  }
}
