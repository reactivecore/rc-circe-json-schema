package net.reactivecore.cjs.resolver

import io.circe.Decoder
import io.circe.generic.semiauto

/** A JSON piece with important data for resolving. */
case class ResolveablePiece(
    `$ref`: Option[RefUri] = None,
    `$id`: Option[RefUri] = None
)

object ResolveablePiece {
  implicit val decoder: Decoder[ResolveablePiece] = semiauto.deriveDecoder[ResolveablePiece]
}
