package net.reactivecore.cjs

import io.circe.Json

case class Vocabulary(
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

  object v2020 {
    val core = VocabularyPart(
      "https://json-schema.org/draft/2020-12/vocab/core",
      Map(
        "$id" -> Full,
        "$schema" -> Full,
        "$ref" -> Full,
        "$anchor" -> Full,
        "$dynamicRef" -> Full,
        "$dynamicAnchor" -> Full,
        "$vocabulary" -> Full,
        "$comment" -> Full,
        "$defs" -> Values
      )
    )

    val applicator = VocabularyPart(
      "https://json-schema.org/draft/2020-12/vocab/applicator",
      Map(
        "prefixItems" -> Single,
        "items" -> Single,
        "contains" -> Single,
        "additionalProperties" -> Single,
        "properties" -> Values,
        "patternProperties" -> Values,
        "dependentSchemas" -> Values,
        "propertyNames" -> Full,
        "if" -> Single,
        "then" -> Single,
        "else" -> Single,
        "allOf" -> Single,
        "anyOf" -> Single,
        "oneOf" -> Single,
        "not" -> Single
      )
    )

    val unevaluated = VocabularyPart.sameKind(
      "https://json-schema.org/draft/2020-12/vocab/unevaluated",
      Single,
      "unevaluatedItems",
      "unevaluatedProperties"
    )

    val validation = VocabularyPart.sameKind(
      "https://json-schema.org/draft/2020-12/vocab/validation",
      Full,
      "type",
      "const",
      "enum",
      "multipleOf",
      "maximum",
      "exclusiveMaximum",
      "minimum",
      "exclusiveMinimum",
      "maxLength",
      "minLength",
      "pattern",
      "maxItems",
      "minItems",
      "uniqueItems",
      "maxContains",
      "minContains",
      "maxProperties",
      "minProperties",
      "required",
      "dependentRequired"
    )

    val metaData = VocabularyPart.sameKind(
      "https://json-schema.org/draft/2020-12/vocab/meta-data",
      Full,
      "title",
      "description",
      "default",
      "deprecated",
      "readOnly",
      "writeOnly",
      "examples"
    )

    val formatAnnotation = VocabularyPart.sameKind(
      "https://json-schema.org/draft/2020-12/vocab/format-annotation",
      Full,
      "format"
    )

    val content = VocabularyPart(
      "https://json-schema.org/draft/2020-12/vocab/content",
      Map(
        "contentEncoding" -> Full,
        "contentMediaType" -> Full,
        "contentSchema" -> Single
      )
    )

    /** All Parts of v2020 */
    val all = Seq(
      core,
      applicator,
      unevaluated,
      validation,
      metaData,
      formatAnnotation,
      content
    )
  }
}
