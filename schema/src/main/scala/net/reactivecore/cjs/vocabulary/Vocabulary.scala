package net.reactivecore.cjs.vocabulary

import io.circe.Codec

/** A vocabulary contains parts */
case class Vocabulary(
    parts: Vector[VocabularyPart]
) {
  def schemaCodec: Codec[Schema] = ???
}

object Vocabulary {

  def default2020: Vocabulary = Vocabulary(
    Vector(CoreVocabulary2020)
  )
}
