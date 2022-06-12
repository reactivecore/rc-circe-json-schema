package net.reactivecore.cjs

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.resolver.RefUri
import net.reactivecore.cjs.util.Codecs
import net.reactivecore.cjs.validator.ValidationProvider

/** Describes location of a Schema. */
case class Location(
    `$schema`: Option[RefUri] = None,
    `$vocabulary`: Option[Map[String, Boolean]] = None,
    `$id`: Option[RefUri] = None
)

object Location {

  implicit val codec: Codec.AsObject[Location] = Codecs.withoutNulls(semiauto.deriveCodec[Location])

  implicit val validationProvider: ValidationProvider[Location] = ValidationProvider.empty
}
