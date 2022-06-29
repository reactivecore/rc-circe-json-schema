package net.reactivecore.cjs.vocabulary

import io.circe.Codec
import io.circe.generic.semiauto
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.resolver.RefUri
import net.reactivecore.cjs.util.{Codecs, VectorMap}
import net.reactivecore.cjs.validator.ValidationProvider

/** A single vocabulary. */
trait Vocabulary {
  val name: String

  type Implementation

  def codec(implicit schemaCodec: Codec[Schema]): Codec[Implementation]

  def validationProvider: ValidationProvider[Implementation]
}
