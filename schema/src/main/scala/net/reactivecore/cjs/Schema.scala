package net.reactivecore.cjs

import cats.MonadError
import cats.implicits._
import io.circe.Codec
import net.reactivecore.cjs.resolver._
import net.reactivecore.cjs.restriction.Restriction
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.{
  BooleanSchemaValidator,
  ObjectSchemaValidator,
  SchemaValidator,
  ValidationProvider,
  Validator
}

import scala.language.higherKinds

/** A JSON Schema */
sealed trait Schema {

  /** Returns a validator for the schema. */
  def validator(parentId: RefUri, path: JsonPointer): Validator = {
    Schema.validationProvider(parentId, path, this)
  }

  /**
    * Returns a [[SchemaValidator]] for this Schema.
    *
    * In contrast to subValdiator, the URI is already given, and the root $id will be ignored.
    */
  def schemaValidator(id: RefUri): SchemaValidator = {
    this match {
      case BooleanSchema(value) => BooleanSchemaValidator(JsonPointer(), value)
      case os: ObjectSchema =>
        ObjectSchema.validatorWithId(id, JsonPointer(), os)
    }
  }

  /** Returns a Description for this Schema. */
  def description: Description

  /** Resolves this schema with all referenced Schemas. */
  def resolve[F[_]](
      downloader: Downloader[F]
  )(implicit applicativeError: MonadError[F, ResolveError]): F[DocumentValidator] = {
    val resolver = new Resolver(downloader)
    val json = Schema.codec.apply(this)

    for {
      resolvedJson <- resolver.resolve(json)
      built <- DocumentValidator.build(resolvedJson) match {
        case Left(err) =>
          applicativeError.raiseError(ResolveError(s"Could not parse resolved json: ${err}"))
        case Right(ok) => applicativeError.pure(ok)
      }
    } yield {
      built
    }
  }

  /** Try to resolve the schema into a validator without downloading anything */
  def emptyResolve: Either[String, DocumentValidator] = {
    resolve(Downloader.emptySimple).left.map(_.message)
  }
}

/** A Schema which directly evaluates into true or false */
case class BooleanSchema(value: Boolean) extends Schema {
  override def description: Description = Description()
}

object BooleanSchema {
  implicit def codec: Codec[BooleanSchema] = SchemaCodec.booleanSchemaCodec

  implicit val validationProvider: ValidationProvider[BooleanSchema] = ValidationProvider.withUri { (_, path, schema) =>
    BooleanSchemaValidator(path, schema.value)
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

  implicit val validationProvider: ValidationProvider[ObjectSchema] = new ValidationProvider[ObjectSchema] {
    override def apply(parentId: RefUri, path: JsonPointer, os: ObjectSchema): Validator = {
      val id = os.location.id
        .map { id =>
          parentId.resolve(id)
        }
        .getOrElse(parentId)

      validatorWithId(id, path, os)
    }
  }

  def validatorWithId(id: RefUri, path: JsonPointer, os: ObjectSchema): ObjectSchemaValidator = {
    val restrictionValidator = Restriction.validationProvider(id, path, os.restriction)
    val refValidator = Ref.validationProvider(id, path, os.ref)
    val locationValidator = Location.validationProvider(id, path, os.location)
    val definitionsValidator = Definitions.validationProvider(id, path, os.definitions)
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
      path,
      underlying,
      fragment = os.ref.anchor,
      dynamicFragment = dynamicAnchor,
      idOverride = os.location.id
    )
  }
}

object Schema {
  implicit def codec: Codec[Schema] = SchemaCodec.schemaCodec

  implicit val validationProvider: ValidationProvider[Schema] = ValidationProvider.withUri {
    case (uri, path, b: BooleanSchema) => BooleanSchema.validationProvider(uri, path, b)
    case (uri, path, r: ObjectSchema)  => ObjectSchema.validationProvider(uri, path, r)
  }

  /** Convenience method for Parsing a Schema */
  def parse(schemaJson: String): Either[io.circe.Error, Schema] = {
    io.circe.parser.parse(schemaJson).flatMap(_.as[Schema])
  }
}
