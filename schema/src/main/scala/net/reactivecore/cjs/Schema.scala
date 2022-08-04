package net.reactivecore.cjs

import io.circe.Codec
import net.reactivecore.cjs.resolver._
import net.reactivecore.cjs.restriction.Restriction
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator._

import scala.language.higherKinds

/** A JSON Schema */
sealed trait Schema {

  /** Returns a validator for the schema. */
  def validator(origin: SchemaOrigin): Validator = {
    Schema.validationProvider(origin, this)
  }

  /**
    * Returns a [[SchemaValidator]] for this Schema.
    *
    * In contrast to subValdiator, the URI is already given, and the root $id will be ignored.
    */
  def schemaValidator(id: RefUri): SchemaValidator = {
    this match {
      case BooleanSchema(value) => BooleanSchemaValidator(SchemaOrigin(id, JsonPointer()), value)
      case os: ObjectSchema =>
        ObjectSchema.validatorWithId(id, SchemaOrigin(id, JsonPointer()), os)
    }
  }

  /** Returns a Description for this Schema. */
  def description: Description
}

/** A Schema which directly evaluates into true or false */
case class BooleanSchema(value: Boolean) extends Schema {
  override def description: Description = Description()
}

object BooleanSchema {
  implicit def codec: Codec[BooleanSchema] = SchemaCodec.booleanSchemaCodec

  implicit val validationProvider: ValidationProvider[BooleanSchema] = ValidationProvider.withOrigin {
    (context, schema) =>
      BooleanSchemaValidator(context, schema.value)
  }
}

/** A Schema in Object form. */
case class ObjectSchema(
    location: Location,
    ref: Ref,
    description: Description,
    definitions: Definitions = Definitions(),
    restriction: Restriction
) extends Schema

object ObjectSchema {
  implicit def codec: Codec.AsObject[ObjectSchema] = Codecs.combineCodecG

  implicit val validationProvider: ValidationProvider[ObjectSchema] = ValidationProvider.withOrigin { (origin, os) =>
    val id = os.location.id
      .map { id =>
        origin.parentId.resolve(id)
      }
      .getOrElse(origin.parentId)

    validatorWithId(id, origin, os)
  }

  def validatorWithId(id: RefUri, origin: SchemaOrigin, os: ObjectSchema): ObjectSchemaValidator = {
    val entered = origin.copy(
      parentId = id
    )
    val restrictionValidator = Restriction.validationProvider(entered, os.restriction)
    val refValidator = Ref.validationProvider(entered, os.ref)
    val locationValidator = Location.validationProvider(entered, os.location)
    val definitionsValidator = Definitions.validationProvider(entered, os.definitions)
    val underlying = Validator.sequence(locationValidator, refValidator, definitionsValidator, restrictionValidator)

    val dynamicAnchor = os.ref.dynamicAnchor.orElse {
      if (os.ref.recursiveAnchor.contains(true)) {
        // For 2019_09, only blank name allowed
        Some("")
      } else {
        None
      }
    }

    ObjectSchemaValidator(
      entered,
      underlying,
      fragment = os.ref.anchor,
      dynamicFragment = dynamicAnchor,
      idOverride = os.location.id
    )
  }
}

object Schema {
  implicit def codec: Codec[Schema] = SchemaCodec.schemaCodec

  implicit val validationProvider: ValidationProvider[Schema] = ValidationProvider.withOrigin {
    case (origin, b: BooleanSchema) => BooleanSchema.validationProvider(origin, b)
    case (origin, r: ObjectSchema)  => ObjectSchema.validationProvider(origin, r)
  }

  /** Convenience method for Parsing a Schema */
  def parse(schemaJson: String): Either[io.circe.Error, Schema] = {
    io.circe.parser.parse(schemaJson).flatMap(_.as[Schema])
  }
}
