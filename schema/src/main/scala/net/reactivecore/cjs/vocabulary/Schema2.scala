package net.reactivecore.cjs.vocabulary

import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}
import net.reactivecore.cjs.{BooleanSchema, Description, ObjectSchema, Schema, SchemaOrigin}
import net.reactivecore.cjs.validator.{BooleanSchemaValidator, SchemaValidator, Validator}

/** Root of a schema */
sealed trait Schema2 {

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
case class BooleanSchema(value: Boolean) extends Schema2 {
  override def validator(origin: SchemaOrigin): SchemaValidator = {
    BooleanSchemaValidator(origin, value)
  }
}

case class ObjectSchema(
    vocabularies: Vocabularies,
    values: Seq[Vocabulary#Implementation]
) extends Schema2 {
  override def validator(origin: SchemaOrigin): SchemaValidator = {
    /*
    Geht so nicht
    vocabularies.vocabularies.zip(values).map { case (vocab, imp) =>
      vocab.validationProvider(origin, imp)
    }
     */
    ???
  }
}
