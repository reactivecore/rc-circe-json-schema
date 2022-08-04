package net.reactivecore.cjs

import io.circe.Json

/**
  * Defines a vocabulary and builds a JSON Filter on top of it.
  * For default vocabularies look into [[Vocabularies]]
  *
  * @param schemaId id of the schema defining this vocabulary
  */
case class Vocabulary(
    schemaId: String,
    parts: Seq[Vocabulary.VocabularyPart]
) {
  private lazy val keywords = parts.flatMap(_.keywords).toMap

  /** Filter JSON input through the vocabulary, only known keywords will survive */
  def filter(input: Json): Json = {
    input.fold(
      Json.Null,
      bool => Json.fromBoolean(bool),
      number => Json.fromJsonNumber(number),
      string => Json.fromString(string),
      array => Json.fromValues(array.map(filter)),
      obj => {
        val filtered = obj.toIterable.collect {
          case (key, value) if keywords.contains(key) =>
            keywords(key) match {
              case Vocabulary.Single =>
                key -> filter(value)
              case Vocabulary.Full =>
                key -> value
              case Vocabulary.Values =>
                key -> value.fold(
                  Json.Null,
                  bool => Json.fromBoolean(bool),
                  number => Json.fromJsonNumber(number),
                  string => Json.fromString(string),
                  array => Json.fromValues(array.map(filter)),
                  o => Json.fromJsonObject(o.mapValues(filter))
                )
            }
        }
        Json.fromFields(filtered)
      }
    )
  }
}

object Vocabulary {

  /** Defines the way a key word should match in JSON */
  sealed trait KeywordKind

  /** Only the keyword should survive, children object, or array elements or object elements are treated as Schema */
  case object Single extends KeywordKind

  /** Values should be treated as schemas */
  case object Values extends KeywordKind

  /** The whole children are treated immutable (e.g. const) */
  case object Full extends KeywordKind

  case class VocabularyPart(
      name: String,
      keywords: Map[String, KeywordKind]
  )

  object VocabularyPart {
    def sameKind(name: String, kind: KeywordKind, keywords: String*): VocabularyPart =
      VocabularyPart(name, keywords.map { k => k -> kind }.toMap)
  }
}
