package net.reactivecore.cjs

import io.circe.syntax._
import io.circe.{Codec, Decoder, Encoder, Json, JsonObject}
import net.reactivecore.cjs.resolver.RefUri
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.ValidationProvider

/** Describes location of a Schema. */
case class Location(
    schema: Option[String] = None,
    id: Option[RefUri] = None
)

object Location {

  implicit val decoder: Decoder[Location] = Decoder { json =>
    for {
      schema <- json.get[Option[String]]("$schema")
      id <- json.get[Option[RefUri]]("$id")
    } yield Location(schema, id)
  }

  implicit val encoder: Encoder.AsObject[Location] = Codecs.withoutNulls {
    Encoder.AsObject.instance[Location] { value =>
      JsonObject(
        "$schema" -> value.schema.asJson,
        "$id" -> value.id.asJson
      )
    }
  }

  implicit lazy val codec: Codec.AsObject[Location] = Codec.AsObject.from(decoder, encoder)

  implicit val validationProvider: ValidationProvider[Location] = ValidationProvider.empty
}
