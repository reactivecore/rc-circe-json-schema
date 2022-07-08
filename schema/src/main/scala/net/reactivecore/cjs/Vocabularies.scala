package net.reactivecore.cjs

object Vocabularies {
  import Vocabulary._

  /** Returns the Vocabulary of Schema 2020_12. */
  def vocabulary2020 = Vocabulary(Vocabularies.v2020.all)

  /** Returns the Vocabulary of Schema 2019_09 */
  def vocabulary2019 = Vocabulary(Vocabularies.v2019.all)

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

  object v2019 {
    val core = VocabularyPart(
      "https://json-schema.org/draft/2019-09/vocab/core",
      Map(
        "$id" -> Full,
        "$schema" -> Full,
        "$anchor" -> Full,
        "$ref" -> Full,
        "$recursiveRef" -> Full,
        "$recursiveAnchor" -> Full,
        "$vocabulary" -> Full,
        "$comment" -> Full,
        "$defs" -> Values
      )
    )

    val applicator = VocabularyPart(
      "https://json-schema.org/draft/2019-09/vocab/applicator",
      Map(
        "additionalItems" -> Single,
        "unevaluatedItems" -> Single,
        "items" -> Values,
        "contains" -> Single,
        "additionalProperties" -> Single,
        "unevaluatedProperties" -> Single,
        "properties" -> Values,
        "patternProperties" -> Values,
        "dependentSchemas" -> Values,
        "propertyNames" -> Single,
        "if" -> Single,
        "then" -> Single,
        "else" -> Single,
        "allOf" -> Single,
        "anyOf" -> Single,
        "oneOf" -> Single,
        "not" -> Single
      )
    )

    val validation = VocabularyPart.sameKind(
      "https://json-schema.org/draft/2019-09/vocab/validation",
      Full,
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
      "dependentRequired",
      "const",
      "enum",
      "type"
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

    val format = VocabularyPart(
      "https://json-schema.org/draft/2019-09/vocab/format",
      Map(
        "format" -> Full
      )
    )

    val content = VocabularyPart(
      "https://json-schema.org/draft/2019-09/vocab/content",
      Map(
        "contentMediaType" -> Full,
        "contentEncoding" -> Full,
        "contentSchema" -> Single
      )
    )

    val all = Seq(
      core,
      applicator,
      validation,
      metaData,
      format,
      content
    )
  }
}
