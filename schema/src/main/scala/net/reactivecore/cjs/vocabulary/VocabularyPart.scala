package net.reactivecore.cjs.vocabulary

import io.circe.{Codec, Decoder, Encoder, JsonObject}
import net.reactivecore.cjs.validator.{ValidationProvider, Validator}
import net.reactivecore.cjs.{Schema, SchemaOrigin}

/** Wraps the parsed value of a vocabulary part. */
trait VocabularyEntity {
  def encode(implicit vocabulary: Vocabulary): JsonObject
  def validator(origin: SchemaOrigin): Validator
}

/** Part of a vocabulary */
trait VocabularyPart {

  /** Name of the vocabulary part. */
  def name: String

  /** Decode an entity */
  def decodeEntity(implicit vocabulary: Vocabulary): Decoder[VocabularyEntity]
}

/** A single port of a vocabulary
  * @tparam T the data holder holding the parsed value
  */
trait TypedVocabularyPart[T] extends VocabularyPart {
  self =>
  def name: String

  def codec(implicit schemaCodec: Codec[Schema]): Codec.AsObject[T]

  override def decodeEntity(implicit vocabulary: Vocabulary): Decoder[VocabularyEntity] =
    codec(vocabulary.schemaCodec).map(Entity)

  def validationProvider: ValidationProvider[T]

  case class Entity(value: T) extends VocabularyEntity {

    override def encode(implicit vocabulary: Vocabulary): JsonObject =
      self.codec(vocabulary.schemaCodec).encodeObject(value)

    override def validator(origin: SchemaOrigin): Validator = self.validationProvider(origin, value)
  }
}
