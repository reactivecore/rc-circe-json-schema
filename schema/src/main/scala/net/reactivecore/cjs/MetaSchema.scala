package net.reactivecore.cjs

import io.circe.Codec
import io.circe.generic.semiauto

/** Contains (part of) information of the Meta Schema. */
case class MetaSchema(
    `$vocabulary`: Option[Map[String, Boolean]] = None
)

object MetaSchema {
  implicit val codec: Codec.AsObject[MetaSchema] = semiauto.deriveCodec
}
